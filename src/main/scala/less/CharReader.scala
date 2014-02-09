package com.kschuetz.less

import java.io.Reader

case class PosChar(c: Char, line: Int, col: Int)

class CharReader(val source: Reader,
                 var line: Int,
                 var col: Int) {

  import scala.collection.mutable.ArrayBuffer

  val bufferSize = 64
  var bufferReadIndex = 0
  val buffer = new ArrayBuffer[PosChar](bufferSize)
  var marks: List[Int] = List.empty[Int]

  private def shiftBuffer(): Unit = {
    if(bufferReadIndex > 0) {
      val itemsToMove = buffer.length - bufferReadIndex
      var i = 0
      while(i < itemsToMove) {
        buffer(i) = buffer(bufferReadIndex + i)
        i += 1
      }
      bufferReadIndex = 0
      buffer.reduceToSize(buffer.length - itemsToMove)
    }
  }

  private def appendToBuffer(item: PosChar): Unit = {
    buffer += item
  }

  def mark: Unit = {
    marks = bufferReadIndex :: marks
  }

  def unmark: Unit = {
    marks = marks.tail
    if(marks.isEmpty) {
      shiftBuffer()
    }
  }

  def rollback: Unit = {
    bufferReadIndex = marks.head
  }

  def get: Option[PosChar] = {
    if(bufferReadIndex < buffer.length) {
      val res = Some(buffer(bufferReadIndex))
      bufferReadIndex += 1
      res
    } else {
      val ci = source.read
      if(ci < 0) None
      else {
        val c = ci.toChar
        c match {
          case '\r' => {
            val c2 = source.read
            if(c2 < 0) None
            else if (c2 == 10) {
              val res = PosChar('\n', line, col)
              if(marks.nonEmpty) appendToBuffer(res)
              line += 1
              col = 1
              Some(res)
            } else {
              val res = PosChar('\n', line, col)
              if(marks.nonEmpty) appendToBuffer(res)
              line += 1
              col = 1
              appendToBuffer(PosChar(c2.toChar, line, col))
              Some(res)
            }
          }

          case '\n' => {
            val res = PosChar('\n', line, col)
            if(marks.nonEmpty) appendToBuffer(res)
            line += 1
            col += 1
            Some(res)
          }

          case _ => {
            val res = PosChar(if(c.isWhitespace) ' ' else c, line, col)
            if(marks.nonEmpty) appendToBuffer(res)
            col += 1
            Some(res)
          }
        }
      }
    }
  }

  def unget(item: PosChar): Unit = {
    if(bufferReadIndex > 0) {
      bufferReadIndex -= 1
    } else {
      buffer.insert(0, item)
    }
  }

  def close(): Unit = {
    source.close()
  }
}
