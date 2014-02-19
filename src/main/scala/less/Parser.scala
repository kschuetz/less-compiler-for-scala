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
      case Token(InterpolatedIdentifier(ident), _) => syntax.StringVarRef(ident)
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




  object DimensionSuffix {
    def unapply(token: Token): Option[syntax.DimensionUnit] = {
      token match {
        case Token(Identifier(s), context) if !context.followsWhitespace =>
          syntax.DimensionUnit.byName(s)
        case _=> None
      }
    }
  }

  val dimensionSuffix: Parser[syntax.DimensionUnit] =
    accept("dimension", {
      case DimensionSuffix(units) => units
    })


  /*val typedNumericValue: Parser[syntax.TypedNumericValue] =
    numericConstant ~
  */

  def atIdent(name: String): Parser[String] =
    accept(s"@${name}", {
      case Token(AtIdentifier(s), _) if s == name => name
    })


  val importDirective: Parser[syntax.ImportDirective] =
    (atIdent("import") ~> stringLiteral <~ semicolon) ^^
      { case s => ImportDirective(s) }

}