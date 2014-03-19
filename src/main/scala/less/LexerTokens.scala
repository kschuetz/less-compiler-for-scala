package com.kschuetz.less


sealed abstract trait TokenValue {
  def isComment: Boolean = false
  def isError: Boolean = false
}

sealed trait ErrorTokenValue extends TokenValue {
  val message: String
  override def isError = true
}

object tokens {
  case object Eq extends TokenValue
  case object Gt extends TokenValue
  case object Lt extends TokenValue
  case object GtEq extends TokenValue
  case object LtEq extends TokenValue
  case object Slash extends TokenValue
  case object Minus extends TokenValue
  case object Plus extends TokenValue
  case object Star extends TokenValue
  case object LBrace extends TokenValue
  case object RBrace extends TokenValue
  case object LParen extends TokenValue
  case object RParen extends TokenValue
  case object LBracket extends TokenValue
  case object RBracket extends TokenValue
  case object Pipe extends TokenValue       // TODO: add Pipe to lexer
  case object Semicolon extends TokenValue
  case object Comma extends TokenValue
  case object Colon extends TokenValue
  case object DotDotDot extends TokenValue
  case object Bang extends TokenValue
  case object Percent extends TokenValue
  case object Ampersand extends TokenValue

  case class WholeNumber(value: BigInt) extends TokenValue
  case class RealNumber(value: String, asDouble: Double) extends TokenValue
  case class BlockComment(value: String) extends TokenValue {
    override def isComment: Boolean = true
  }
  case class InlineComment(value: String) extends TokenValue {
    override def isComment: Boolean = true
  }

  case object SingleQuoteLiteral extends TokenValue
  case object DoubleQuoteLiteral extends TokenValue
  case object TildeQuoteLiteral extends TokenValue
  case class StringLiteralChunk(value: String) extends TokenValue
  case class InterpolatedIdentifier(name: String) extends TokenValue
  case object StringLiteralEnd extends TokenValue

  case class UnquotedString(value: String) extends TokenValue


  case object Backslash extends TokenValue

  case object Includes extends TokenValue
  case object DashMatch extends TokenValue
  case object PrefixMatch extends TokenValue
  case object SuffixMatch extends TokenValue
  case object SubstringMatch extends TokenValue

  case class Identifier(name: String) extends TokenValue
  case class DotIdentifier(name: String) extends TokenValue

  case class HashIdentifier(name: String) extends TokenValue {
    val asHexString: Option[String] = {
      if(name.forall { c => c.isDigit || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')}) {
        Some(name.toLowerCase)
      } else None
    }
    val isValidColorHex = ((name.length == 3) || (name.length == 6)) && asHexString.isDefined
  }

  case class AtIdentifier(name: String) extends TokenValue
  case class AtAtIdentifier(name: String) extends TokenValue
  case class AtBraceIdentifier(name: String) extends TokenValue
  case class DotAtBraceIdentifier(name: String) extends TokenValue

  case object Url extends TokenValue
  case object BadUrl extends TokenValue

  case class Unknown(value: String) extends TokenValue



  // error tokens


  case class LexerError(message: String) extends ErrorTokenValue


}

