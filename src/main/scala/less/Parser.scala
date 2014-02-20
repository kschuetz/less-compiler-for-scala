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
    (urlExpressionOpen ~> urlExpressionValue <~ token(")", RParen))


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



  def atIdent(name: String): Parser[String] =
    accept(s"@${name}", {
      case Token(AtIdentifier(s), _) if s == name => name
    })

  val varRef: Parser[syntax.VarRef] =
    accept(s"variable reference", {
      case Token(AtIdentifier(name), _) => syntax.DirectVarRef(name)
      case Token(AtAtIdentifier(name), _) => syntax.IndirectVarRef(syntax.DirectVarRef(name))
    })


  val importDirective: Parser[syntax.ImportDirective] =
    (atIdent("import") ~> stringLiteral <~ semicolon) ^^
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


  val lParen = token("(", LParen)
  val rParen = token(")", RParen)

  val factor: Parser[Expr] =
    typedNumericValue |
    varRef |
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

  val componentValueList: Parser[ComponentValueList] =
    componentValue ~ rep(componentValue) ^^ {
      case x ~ xs => ComponentValueList(x, xs)
    }

  val argument: Parser[Argument] =
    (identifier ~ token("=", Eq) ~ opt(componentValueList)) ^^ {
      case ident ~ _ ~ cvl => Argument(Some(ident), cvl)
    } |
    componentValueList ^^ {
      case cvl => Argument(None, Some(cvl))
    }

  val functionApplication: Parser[FunctionApplication] = ???

}