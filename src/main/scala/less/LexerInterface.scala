package com.kschuetz.less

import source._
import java.io.Reader
import scala.util.parsing.input.Position


case class TokenContext(position: Position,
                        followsWhitespace: Boolean = false,   // some type of whitespace (spaces, tabs, newlines, or comments) has occurred since last token
                        onNewLine: Boolean = false,           // at least one new line has been crossed since last token
                        immediatelyFollowsComment: Boolean = false) {   // token occurs _immediately_ after the end of a block comment, with no whitespace between)

  def isAtLineBegin: Boolean =
    position.column == 1
}

case class Token(value: TokenValue, context: TokenContext) {

  def isError: Boolean = value.isError

  override def toString = {
    val wFlag = if(context.followsWhitespace) "w" else ""
    val nFlag = if(context.onNewLine) "n" else ""
    val cFlag = if(context.immediatelyFollowsComment) "c" else ""
    s"<$value @ ${context.position} $wFlag$nFlag$cFlag>"
  }
}



trait Lexer extends ((Reader, (Int, Int) => Position) => Stream[Token])

