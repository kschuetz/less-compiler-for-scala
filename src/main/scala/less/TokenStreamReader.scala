package com.kschuetz.less

import scala.util.parsing.input.{Position, Reader}


class TokenStreamReader(source: Stream[Token]) extends Reader[Token] {

  def first: Token = source.head

  def rest: Reader[Token] = new TokenStreamReader(source.tail)

  def pos: Position = source.head.context.position

  def atEnd: Boolean = source.isEmpty

}