package com.kschuetz.less

import util.parsing.combinator._


trait LessParsers extends Parsers {

  import tokens._
  import syntax._

  type Elem = Token


  def token(value: TokenValue): Parser[Token] =
    acceptIf(_.value == value){ elem =>
      s"${value} expected"
    }

  def token(kind: String, value: TokenValue): Parser[Token] =
    acceptIf(_.value == value){ elem => s"${kind} expected"}


  val semicolon = token(";", Semicolon)
  val lParen = token("(", LParen)
  val rParen = token(")", RParen)
  val comma = token(",", Comma)
  val colon = token(":", Colon)
  val equals = token("=", Eq)
  val bang = token("!", Bang)
  val lBracket = token("[", LBracket)
  val rBracket = token("]", RBracket)
  val pipe = token("|", Pipe)
  val star = token("*", Star)

  val stringLiteralOpen: Parser[syntax.QuoteDelimiter] = accept("opening quote", {
    case Token(DoubleQuoteLiteral, _) => syntax.DoubleQuoteDelimiter
    case Token(SingleQuoteLiteral, _) => syntax.SingleQuoteDelimiter
    case Token(TildeQuoteLiteral, _) => syntax.TildeQuoteDelimiter
  })

  val stringLiteralChunk: Parser[syntax.StringValue] =
    accept("string literal", {
      case Token(StringLiteralChunk(s), _) => syntax.StringConstant(s)
      case Token(InterpolatedIdentifier(ident), _) => syntax.DirectVarRef(ident)
    })

  val stringLiteralClose = token("closing quote", StringLiteralEnd)

  val stringLiteral: Parser[syntax.StringLiteral] =
    (stringLiteralOpen ~ rep(stringLiteralChunk) <~ stringLiteralClose) ^^
      { case q ~ chunks => syntax.StringLiteral(q, chunks) }


  val urlExpressionOpen = token("url(", Url)

  val urlExpressionValue: Parser[syntax.UrlExpression] =
    (stringLiteral ^^ { case s => syntax.UrlQuoted(s) }) |
    accept("url", {
      case Token(UnquotedString(s), _) => syntax.UrlUnquoted(s)
    })


  val urlExpression: Parser[syntax.UrlExpression] =
    (urlExpressionOpen ~> urlExpressionValue <~ rParen)


  val numericConstant: Parser[syntax.NumericConstant] =
    accept("number", {
      case Token(tokens.WholeNumber(n), _) => syntax.WholeNumber(n)
      case Token(tokens.RealNumber(s, n), _) => syntax.RealNumber(n, s)
    })



  sealed abstract trait NumberSuffix
  case object PercentSuffix extends NumberSuffix
  case class DimensionSuffix(units: syntax.DimensionUnit) extends NumberSuffix

  object ExtractPercentSuffix {
    def unapply(token: Token): Option[NumberSuffix] = {
      token match {
        case Token(Percent, context) if !context.followsWhitespace => Some(PercentSuffix)
        case _ => None
      }
    }
  }

  object ExtractDimensionSuffix {
    def unapply(token: Token): Option[NumberSuffix] = {
      token match {
        case Token(Identifier(s), context) if !context.followsWhitespace =>
          syntax.DimensionUnit.byName(s).map(DimensionSuffix(_))
        case _=> None
      }
    }
  }

  val numberSuffix: Parser[NumberSuffix] =
    accept("number suffix", {
      case ExtractDimensionSuffix(ds) => ds
      case ExtractPercentSuffix(ps) => ps
    })



  val typedNumericValue: Parser[syntax.TypedNumericValue] =
     numericConstant ~ opt(numberSuffix) ^^ {
       case n ~ Some(DimensionSuffix(units)) => syntax.Dimension(n, units)
       case n ~ Some(PercentSuffix) => Percentage(n)
       case n ~ _ => syntax.TypedNumericValue(n)
     }



  def atKeyword(name: String): Parser[String] =
    accept(s"@${name}", {
      case Token(AtIdentifier(s), _) if s == name => name
    })

  def keyword(name: String): Parser[String] =
    accept(s"${name}", {
      case Token(Identifier(s), _) if s == name => name
  })

  val priority: Parser[syntax.Priority] =
    bang ~ keyword("important") ^^^ syntax.Important


  val atIdent: Parser[String] =
    accept("variable name", {
      case Token(AtIdentifier(name), _) => name
    })

  val dotIdent: Parser[String] =
    accept(".identifier", {
      case Token(DotIdentifier(name), _) => name
    })

  val varRef: Parser[syntax.VarRef] =
    accept("variable reference", {
      case Token(AtIdentifier(name), _) => syntax.DirectVarRef(name)
      case Token(AtAtIdentifier(name), _) => syntax.IndirectVarRef(syntax.DirectVarRef(name))
    })


  val importDirective: Parser[syntax.ImportDirective] =
    (atKeyword("import") ~> stringLiteral <~ semicolon) ^^
      { case s => ImportDirective(s) }




  val add = syntax.Add.apply _
  val subtract = syntax.Subtract.apply _
  val multiply = syntax.Multiply.apply _
  val divide = syntax.Divide.apply _

  val addOp: Parser[(Expr, Expr) => Expr] =
    accept("add op", {
      case Token(Plus, _) => add
      case Token(Minus, _) => subtract
    })

  val mulOp: Parser[(Expr, Expr) => Expr] =
    accept("mul op", {
      case Token(Star, _) => multiply
      case Token(Slash, _) => divide
    })


  val factor: Parser[Expr] =
    typedNumericValue |
    varRef |
    functionApplication |
    lParen ~> expr <~ rParen

  val term: Parser[Expr] =
    chainl1(factor, mulOp)

  val expr: Parser[Expr] =
    chainl1(term, addOp)

  val identifier: Parser[String] =
    accept("identifier", {
      case Token(Identifier(name), _) => name
    })

  val bareIdentifier: Parser[syntax.BareIdentifier] =
    identifier ^^ { case name => syntax.BareIdentifier(name) }

  val componentValue: Parser[ComponentValue] =
    expr |
    stringLiteral |
    urlExpression |
    bareIdentifier

  val valueVector: Parser[ValueVector] =
    rep(componentValue) ^^ {
      case xs => ValueVector(xs)
    }

  val moreValueVectors: Parser[List[ValueVector]] =
    rep(comma ~> opt(valueVector) ^^ {
      case Some(v) => v
      case None => ValueVector.empty
    })

  val valueVectorList: Parser[List[ValueVector]] =
    opt(valueVector) ~ moreValueVectors ^^ {
      case Some(value) ~ moreValues => value :: moreValues
      case None ~ moreValues => ValueVector.empty :: moreValues
    }

  val varDeclaration: Parser[VarDeclaration] =
    (atIdent <~ colon) ~ valueVectorList <~ semicolon ^^ {
      case name ~ values => VarDeclaration(name, values)
    }

  val argument: Parser[Argument] =
    (identifier <~ equals) ~ opt(valueVector) ^^ {
      case ident ~ cvl => Argument(Some(ident), cvl)
    } |
    valueVector ^^ {
      case cvl => Argument(None, Some(cvl))
    }

  val moreArguments: Parser[List[Argument]] = {
    rep(comma ~> opt(argument) ^^ {
      case Some(arg) => arg
      case None => Argument(None, None)
    })
  }

  val argumentList: Parser[List[Argument]] = {
    (argument ~ moreArguments) ^^ {
      case x ~ xs => x :: xs
    } | moreArguments
  }

  val functionApplication: Parser[FunctionApplication] =
    ((identifier <~ lParen) ~ (argumentList <~ rParen)) ^^ {
      case name ~ args => FunctionApplication(name, args)
    }

  val mixinApplication: Parser[MixinApplication] =
    dotIdent ~ opt(lParen ~> argumentList <~ rParen) ^^ {
      case name ~ Some(args) => MixinApplication(name, args)
      case name ~ None => MixinApplication(name, Nil)
    }

  // selectors




  val firstSelectorIdentChunk: Parser[syntax.StringValue] =
    accept("selector ident chunk", {
      case Token(AtBraceIdentifier(name), _) => syntax.DirectVarRef(name)
      case Token(Identifier(name), _) => StringConstant(name)
    })

  val firstDotSelectorIdentChunk: Parser[syntax.StringValue] =
    accept("selector dot ident chunk", {
      case Token(AtBraceIdentifier(name), _) => syntax.DirectVarRef(name)
      case Token(DotIdentifier(name), _) => syntax.StringConstant(name)
    })

  val firstHashSelectorIdentChunk: Parser[syntax.StringValue] =
    accept("selector dot ident chunk", {
      case Token(AtBraceIdentifier(name), _) => syntax.DirectVarRef(name)
      case Token(HashIdentifier(name), _) => syntax.StringConstant(name)
    })

  val anotherSelectorIdentChunk: Parser[syntax.StringValue] =
    accept("selector ident chunk", {
      case Token(AtBraceIdentifier(name), ctx) if !ctx.followsWhitespace => syntax.DirectVarRef(name)
      case Token(Identifier(name), ctx) if !ctx.followsWhitespace => StringConstant(name)
    })

  val moreSelectorIdentChunks = rep(anotherSelectorIdentChunk)

  val selectorIdent: Parser[CompositeIdentifier] =
    firstSelectorIdentChunk ~ moreSelectorIdentChunks ^^ {
      case x ~ xs => CompositeIdentifier(x :: xs)
    }

  val dotSelectorIdent: Parser[CompositeIdentifier] =
    firstDotSelectorIdentChunk ~ moreSelectorIdentChunks ^^ {
      case x ~ xs => CompositeIdentifier(x :: xs)
    }

  val hashSelectorIdent: Parser[CompositeIdentifier] =
    firstHashSelectorIdentChunk ~ moreSelectorIdentChunks ^^ {
      case x ~ xs => CompositeIdentifier(x :: xs)
    }

  val namespacePrefix: Parser[NamespaceComponent] =
    pipe ^^^ { NoNamespace } |
    star ~ pipe ^^^ { AnyNamespace }  |
    selectorIdent <~ pipe ^^ { case ident => Namespace(ident) }

  val typeSelector: Parser[TypeSelector] =
    namespacePrefix ~ selectorIdent ^^ { case n ~ elem => TypeSelector(elem, n)} |
    selectorIdent ^^ { case elem => TypeSelector(elem, DefaultNamespace) }

  val universalSelector: Parser[UniversalSelector] =
    namespacePrefix <~ star ^^ { case n => UniversalSelector(n) } |
    star ^^^ UniversalSelector(DefaultNamespace)

  val classSelector: Parser[ClassSelector] =
    dotSelectorIdent ^^ { case ident => ClassSelector(ident) }

  val idSelector: Parser[IDSelector] =
    hashSelectorIdent ^^ { case ident => IDSelector(ident) }


  //val attributeValue: Parser[AttributeValue] =


  //val attributeSelector: Parser[AttributeSelector] =
  //  lBracket ~ opt(namespacePrefix) ~ selectorIdent


  //val selectorIdent: Parser[SelectorIdent] =


  //val namespacePrefix =
   // star ~ pipe

 // val selectorIdent: Parser[SelectorIdent] =


  //val attributeSelector: Parser[AttributeSelector] =


  //val selectorGroup: Parser[List[Selector]]


}