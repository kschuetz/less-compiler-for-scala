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



}