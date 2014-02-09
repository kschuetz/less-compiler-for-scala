package com.kschuetz.less


sealed abstract trait Token {
  def isComment: Boolean = false
}

object tokens {

  case object Eq extends Token
  case object Gt extends Token
  case object Slash extends Token
  case object Minus extends Token
  case object Plus extends Token
  case object Star extends Token
  case object LBrace extends Token
  case object RBrace extends Token
  case object LParen extends Token
  case object RParen extends Token
  case object LBracket extends Token
  case object RBracket extends Token
  case object Semicolon extends Token
  case object Comma extends Token
  case object Dot extends Token
  case object Hash extends Token
  case object Bang extends Token
  case object Percent extends Token
  case object At extends Token
  case object Ampersand extends Token
  case class Identifier(name: String) extends Token
  case class WholeNumber(value: BigInt) extends Token
  case class FloatNumber(value: Double) extends Token
  case class BlockComment(value: String) extends Token {
    override def isComment: Boolean = true
  }
  case class InlineComment(value: String) extends Token {
    override def isComment: Boolean = true
  }

  case class Colon(rightPadded: Boolean) extends Token


  case class StringLiteralChunk(value: String) extends Token
  case class InterpolatedIdentifier(name: String) extends Token



  case object Includes extends Token
  case object DashMatch extends Token
  case object PrefixMatch extends Token
  case object SuffixMatch extends Token
  case object SubstringMatch extends Token

}

