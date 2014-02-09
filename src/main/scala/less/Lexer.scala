package com.kschuetz.less

import source._
import java.io.Reader


object LessLexer {

  object S_ extends Enumeration {
    type S_ = Value

    val START, NUMBER, AT, SLASH, LINECOMMENT, BLOCKCOMMENT = Value
  }

}

class LessLexer extends Lexer {



  sealed abstract trait TokenResult
  case class Success(token: LexerToken) extends TokenResult
  case class SuccessMany(tokens: Vector[LexerToken]) extends TokenResult
  case class Failure(error: LexerError) extends TokenResult
  case object NothingLeft extends TokenResult




  class State(reader: CharReader,
              makeSourcePos: (Int, Int) => Position) {



    private def scan: TokenResult = {
      import LessLexer.S_._

      var state = START
      var found = false
      var result: TokenResult = NothingLeft
      var startLine: Int = 0
      var startCol: Int = 0

      val capture = new StringBuilder

      def accept(token: Token, line: Int, col: Int): Unit = {
        result = Success(LexerToken(token, makeSourcePos(line, col)))
        found = true
      }

      def error(msg: String, line: Int, col: Int): Unit = {
        val e = ParseError(msg, makeSourcePos(line, col))
        result = Failure(e)
      }

      def markBegin(line: Int, col: Int) = {
        startLine = line
        startCol = col
      }

      def startCapture(c: Char, line: Int, col: Int) = {
        capture.clear()
        capture.append(c)
        startLine = line
        startCol = col
      }

      while(!found) {
        reader.get.map { case pc @ PosChar(c, line, col) =>
          import tokens._

          if(state == START) {
            c match {
              case '>' =>   accept(Gt, line, col)
              case '{' =>   accept(LBrace, line, col)
              case '}' =>   accept(RBrace, line, col)
              case '[' =>   accept(LBracket, line, col)
              case ']' =>   accept(RBracket, line, col)
              case '=' =>   accept(Eq, line, col)
              case ';' =>   accept(Semicolon, line, col)
              case ':' =>   accept(Colon, line, col)
              case '-' =>   accept(Minus, line, col)
              case '+' =>   accept(Plus, line, col)
              case '*' =>   accept(Star, line, col)
              case '(' =>   accept(LParen, line, col)
              case ')' =>   accept(RParen, line, col)
              case ',' =>   accept(Comma, line, col)
              case '.' =>   accept(Dot, line, col)
              case '#' =>   accept(Hash, line, col)
              case n if n.isDigit => {
                state = NUMBER
                startCapture(c, line, col)
              }
              case '@' => {
                state = AT
                startCapture(c, line, col)
              }
              case '/' => {
                state = SLASH
                markBegin(line, col)
              }
            }

          } else if (state == AT) {


            
          } else if (state == SLASH) {
            // slash
            c match {
              case '/' => {
                state = LINECOMMENT
                capture.clear()
              }
              case '*' => {
                state = BLOCKCOMMENT
                capture.clear()
              }
              case _ => {
                reader.unget(pc)
                state = START
                accept(Slash, startLine, startCol)
              }
            }
          }
            
        } getOrElse {
          found = true
        }
      }
      result
    }

    def next: Stream[Either[LexerError, LexerToken]] = {
      scan match {
        case Success(t) => {
          Right(t) #:: (new State(reader, makeSourcePos)).next
        }
        case SuccessMany(ts) => {
          ts.foldRight((new State(reader, makeSourcePos)).next){ Right(_) #:: _ }
        }
        case Failure(e) => {
          reader.close()
          Left(e) #:: Stream.empty
        }
        case NothingLeft => {
          reader.close()
          Stream.empty
        }
      }
    }
  }



  def apply(reader: Reader, makeSourcePos: (Int, Int) => Position): Stream[Either[LexerError, LexerToken]] = {
    val charReader = new CharReader(reader, 1, 1)
    (new State(charReader, makeSourcePos)).next
  }

}

