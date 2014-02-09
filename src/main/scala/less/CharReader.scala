package com.kschuetz.less

import java.io.Reader


class CharReader_New(val source: Reader,
                     private var line: Int = 1,
                     private var col: Int = 1) {

  private var pushed: List[SourceChar] = Nil

  private var buffer: Option[Int] = None

  private def getNextChar: Int = {
    if(buffer.isEmpty) {
      source.read
    } else {
      val res = buffer.get
      buffer = None
      res
    }
  }

  def get: Option[SourceChar] = {
    if(pushed.nonEmpty) {
      pushed = pushed.tail
      Some(pushed.head)
    } else {
      val ci = getNextChar
      if(ci < 0) None
      else {
        val c = ci.toChar
        c match {
          case '\r' => {
            val res = Some(SourceChar('\n', line, col))
            line += 1
            col = 1
            val c2 = source.read
            if(c2 >= 0 && c2 != 10) {
              buffer = Some(c2)
            }
            res
          }

          case '\n' => {
            val res = Some(SourceChar('\n', line, col))
            line += 1
            col = 1
            res
          }

          case _ => {
            val res = Some(SourceChar(c, line, col))
            col += 1
            res
          }
        }
      }
    }
  }

  def unget(item: SourceChar): Unit = {
    pushed = item :: pushed
  }

  def lineNumber: Int = {
    pushed.headOption.map { _.line } getOrElse line
  }

  def colNumber: Int = {
    pushed.headOption.map { _.col } getOrElse col
  }

  def close(): Unit = {
    source.close()
  }

}






class CharReader(val source: Reader,
                 var line: Int,
                 var col: Int) {

  import scala.collection.mutable.ArrayBuffer

  val bufferSize = 64
  var bufferReadIndex = 0
  val buffer = new ArrayBuffer[SourceChar](bufferSize)
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

  private def appendToBuffer(item: SourceChar): Unit = {
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

  def get: Option[SourceChar] = {
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
              val res = SourceChar('\n', line, col)
              if(marks.nonEmpty) appendToBuffer(res)
              line += 1
              col = 1
              Some(res)
            } else {
              val res = SourceChar('\n', line, col)
              if(marks.nonEmpty) appendToBuffer(res)
              line += 1
              col = 1
              appendToBuffer(SourceChar(c2.toChar, line, col))
              Some(res)
            }
          }

          case '\n' => {
            val res = SourceChar('\n', line, col)
            if(marks.nonEmpty) appendToBuffer(res)
            line += 1
            col += 1
            Some(res)
          }

          case _ => {
            val res = SourceChar(if(c.isWhitespace) ' ' else c, line, col)
            if(marks.nonEmpty) appendToBuffer(res)
            col += 1
            Some(res)
          }
        }
      }
    }
  }

  def unget(item: SourceChar): Unit = {
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
