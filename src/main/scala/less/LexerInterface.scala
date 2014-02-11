package com.kschuetz.less

import source._
import java.io.Reader

sealed abstract trait LexerError
abstract trait PositionedLexerError extends LexerError {
  def position: Position
}

case class ParseError(message: String, position: Position) extends PositionedLexerError

case class LexerToken(value: Token,                         // token type and lexeme
                      position: Position,
                      followsWhitespace: Boolean = false,   // some type of whitespace (spaces, tabs, newlines, or comments) has occurred since last token
                      onNewLine: Boolean = false,           // at least one new line has been crossed since last token
                      immediatelyFollowsComment: Boolean = false) {   // token occurs _immediately_ after the end of a block comment, with no whitespace between

  def isAtLineBegin: Boolean =
    position.columnNumber == 1

  override def toString = {
    s"<$value @ $position>"
  }
}



trait Lexer extends ((Reader, (Int, Int) => Position) => Stream[Either[LexerError, LexerToken]])

