package com.kschuetz.less

object source {

  abstract trait Position {
    def lineNumber: Int
    def columnNumber: Int
  }

  case class FileSourcePosition(file: java.io.File, lineNumber: Int, columnNumber: Int) extends Position {
    override def toString = {
      s"${file.getName()}:$lineNumber:$columnNumber"
    }
  }

  case class StringSourcePosition(lineNumber: Int, columnNumber: Int) extends Position


  object FileSourcePosition {

    def factory(file: java.io.File): (Int, Int) => Position =
      FileSourcePosition(file, _, _)


  }

  object StringSourcePosition {

    def factory: (Int, Int) => Position =
      StringSourcePosition(_, _)

  }

}