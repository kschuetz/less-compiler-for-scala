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
            case _ => true
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
    val testFile1 = "E:/projects/apps/web/MusicLibraryExplorer/client/bower_components/bootstrap/less/mixins.less"
    val testFile2 = "E:/projects/apps/web/MusicLibraryExplorer/client/app/main.less"
    val testFile3 = "E:/projects/apps/web/MusicLibraryExplorer/client/bower_components/jquery/jquery.js"
    val testFile4 = "C:/Temp/less-sandbox/app/assets/stylesheets/main.less"

    compileFile(new File(testFile4))
  }
}