package com.kschuetz.less

import java.io._

object RunCompiler {

  def compileFile(file: java.io.File): Unit = {
    val reader = new BufferedReader(new FileReader(file))
    val lexer = LessLexer(reader, source.FileSourcePosition.factory(file))
    val items = lexer.toList
    var hasErrors = false
    items.foreach { maybeToken =>
      maybeToken match {
        case Right(t) => {
          val doPrint = t.value match {
            case tokens.Unknown(_) => true
            case _ => false
          }
          if(doPrint) {
            println(t.toString)
          }
        }
        case Left(e) => {
          hasErrors = true
          println("error:" + e.toString)
        }
      }

    }
    if(!hasErrors) println("No errors!")
  }


  def main(args : Array[String]) : Unit = {
    println("Hello world!")
    val testFile1 = "E:/projects/apps/web/MusicLibraryExplorer/client/bower_components/bootstrap/less/forms.less"
    compileFile(new File(testFile1))
  }
}