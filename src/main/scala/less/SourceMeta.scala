package com.kschuetz.less

object source {

  abstract trait Position {
    def line: Int
    def column: Int
  }

  case class FileSourcePosition(file: java.io.File, line: Int, column: Int) extends Position {
    override def toString = {
      s"${file.getName()}:$line:$column"
    }
  }

  case class StringSourcePosition(line: Int, column: Int) extends Position


  object FileSourcePosition {

    def factory(file: java.io.File): (Int, Int) => Position =
      FileSourcePosition(file, _, _)


  }

  object StringSourcePosition {

    def factory: (Int, Int) => Position =
      StringSourcePosition(_, _)

  }

}