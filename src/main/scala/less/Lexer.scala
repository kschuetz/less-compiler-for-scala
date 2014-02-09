package com.kschuetz.less

import source._
import java.io.Reader


class LessLexer extends Lexer {

  sealed abstract trait TokenResult
  case class Success(token: LexerToken) extends TokenResult
  case class Failure(error: LexerError) extends TokenResult
  case object NothingLeft extends TokenResult


  class State(reader: CharReader,
              makeSourcePos: (Int, Int) => Position) {



    private def scan: TokenResult = {
      var found = false
      var result: TokenResult = NothingLeft

      def accept(token: Token, line: Int, col: Int): Unit = {
        result = Success(LexerToken(token, makeSourcePos(line, col)))
        found = true
      }

      def error(msg: String, line: Int, col: Int): Unit = {
        val e = ParseError(msg, makeSourcePos(line, col))
        result = Failure(e)
      }

      while(!found) {
        reader.get.map { case PosChar(c, line, col) =>
          import tokens._

          c match {
            case '>' =>   accept(Gt, line, col)
            case '{' =>   accept(LBrace, line, col)
            case '}' =>   accept(RBrace, line, col)
            case '[' =>   accept(LBracket, line, col)
            case ']' =>   accept(RBracket, line, col)
            case '=' =>   accept(Eq, line, col)
            case ';' =>   accept(Semicolon, line, col)
            case ':' =>   accept(Colon, line, col)
            case '/' =>   accept(Slash, line, col)
            case '-' =>   accept(Minus, line, col)
            case '+' =>   accept(Plus, line, col)
            case '*' =>   accept(Star, line, col)
            case '(' =>   accept(LParen, line, col)
            case ')' =>   accept(RParen, line, col)
            case ',' =>   accept(Comma, line, col)
            case '.' =>   accept(Dot, line, col)
            case '#' =>   accept(Hash, line, col)
            case n if n.isDigit => {



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
        case Success(t) =>
          Right(t) #:: (new State(reader, makeSourcePos)).next
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

