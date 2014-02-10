package com.kschuetz.less

import source._
import java.io.Reader

sealed abstract trait LexerError
abstract trait PositionedLexerError extends LexerError {
  def position: Position
}

case class ParseError(message: String, position: Position) extends PositionedLexerError

case class LexerToken(value: Token, position: Position) {
  override def toString = {
    s"<$value @ $position>"
  }
}



trait Lexer extends ((Reader, (Int, Int) => Position) => Stream[Either[LexerError, LexerToken]])

