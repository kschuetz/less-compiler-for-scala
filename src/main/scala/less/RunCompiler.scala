package com.kschuetz.less

import java.io._

object RunCompiler {

  def compileFile(file: java.io.File): Unit = {
    val reader = new BufferedReader(new FileReader(file))
    val lexer = LessLexer(reader, source.FileSourcePosition.factory(file))
    val items = lexer.toList
    items.foreach { maybeToken =>
      maybeToken match {
        case Right(t) => print(t.toString)
        case Left(e) => print("error:" + e.toString)
      }

    }
  }


  def main(args : Array[String]) : Unit = {
    println("Hello world!")
    val testFile1 = "E:/projects/apps/web/MusicLibraryExplorer/client/bower_components/bootstrap/less/forms.less"
    compileFile(new File(testFile1))
  }
}