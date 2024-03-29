package com.kschuetz.less

import java.io.Reader


class CharReader(val source: Reader,
                 private var nextPosLine: Int = 1,
                 private var nextPosCol: Int = 1) {

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
      val res = Some(pushed.head)
      pushed = pushed.tail
      res
    } else {
      val ci = getNextChar
      if(ci < 0) None
      else {
        val c = ci.toChar
        c match {
          case '\r' => {
            val res = Some(SourceChar('\n', nextPosLine, nextPosCol))
            nextPosLine += 1
            nextPosCol = 1
            val c2 = source.read
            if(c2 >= 0 && c2 != 10) {
              buffer = Some(c2)
            }
            res
          }

          case '\n' => {
            val res = Some(SourceChar('\n', nextPosLine, nextPosCol))
            nextPosLine += 1
            nextPosCol = 1
            res
          }

          case _ => {
            val res = Some(SourceChar(c, nextPosLine, nextPosCol))
            nextPosCol += 1
            res
          }
        }
      }
    }
  }

  def unget(item: SourceChar): Unit = {
    pushed = item :: pushed
  }

  def line: Int = {
    pushed.headOption.map { _.line } getOrElse nextPosLine
  }

  def col: Int = {
    pushed.headOption.map { _.col } getOrElse nextPosCol
  }

  def close(): Unit = {
    source.close()
  }

}


trait Markable extends CharReader {

  private var memory: List[SourceChar] = Nil
  private var marks: List[Int] = Nil
  private var memorySize: Int = 0

  override def get: Option[SourceChar] = {
    var result = super.get
    if(marks.nonEmpty) result.foreach { sc =>
      memorySize += 1
      memory = sc :: memory
    }
    result
  }

  def mark(): Unit = {
    marks = memorySize :: marks
  }

  def unmark(): Unit = {
    val targetSize = marks.head
    memory = memory.drop(memorySize - targetSize)
    memorySize = targetSize
    marks = marks.tail
  }

  def rewind(): Unit = {
    val targetSize = marks.head
    while(memorySize > targetSize) {
      unget(memory.head)
      memorySize -= 1
    }
  }

}





