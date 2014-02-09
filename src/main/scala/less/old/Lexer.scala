package com.kschuetz.less
package old

import source._
import java.io.Reader


object LessLexerState {

  object S_ extends Enumeration {
    type S_ = Value

    val START, NUMBER, NUMDOT, FLOAT, AT, AT_IDENT, SLASH,
        LINECOMMENT, BLOCKCOMMENT, BLOCKCOMMENTSTAR,
        STRING_LIT, LIT_ESC, LIT_AT, LIT_AT_BRACE, LIT_INTERP_IDENT, LIT_INTERP_IDENT_END  = Value
  }

}

class LessLexerState(reader: CharReader, makeSourcePos: (Int, Int) => Position) {


  sealed abstract trait TokenResult
  case class Success(token: LexerToken) extends TokenResult
  case class SuccessMany(tokens: Vector[LexerToken]) extends TokenResult
  case class Failure(error: LexerError) extends TokenResult
  case object NothingLeft extends TokenResult


  def isValidIdentChar(c: Char) = {
    c.isLetterOrDigit || (c == '-') || (c == '_')
  }

  def isValidIdentStartChar(c: Char) =
    isValidIdentChar(c)

  def isLineBreak(c: Char) =
    (c == '\r') || (c == '\n')

  def isStringLiteralDelimiter(c: Char) =
    (c == '\'') || (c == '"')



  def lexerToken(token: Token, line: Int, col: Int): LexerToken =
    LexerToken(token, makeSourcePos(line, col))

  private def scan: TokenResult = {
    import LessLexerState.S_._

    type StateType = LessLexerState.S_.Value

    var state = START
    var found = false
    var result: TokenResult = NothingLeft
    var startLine: Int = 0
    var startCol: Int = 0
    var itemCount: Int = 0
    var subItemOffset: Int = 0;
    var qChar: Char = '\0';


    val capture = new StringBuilder

    val tokenBuffer = new collection.mutable.ArrayBuffer[LexerToken]


    def accept(token: Token, line: Int, col: Int): Unit = {
      result = Success(LexerToken(token, makeSourcePos(line, col)))
      found = true
    }

    def acceptMany(tokens: Vector[LexerToken]): Unit = {
      result = SuccessMany(tokens)
      found = true
    }

    def error(msg: String, line: Int, col: Int): Unit = {
      val e = ParseError(msg, makeSourcePos(line, col))
      result = Failure(e)
      found = true
    }

    def markBegin(line: Int, col: Int) = {
      startLine = line
      startCol = col
    }

    def startCapture(c: Char) = {
      capture.clear()
      capture.append(c)
    }

    def unterminatedBlockCommentError(line: Int, col: Int): Unit =
      error("Unterminated block comment", line, col)


    def unterminatedStringLiteralError(line: Int, col: Int): Unit =
      error("Unterminated string literal", line, col)

    def addStringLiteralChunks(): Unit = {
      import tokens._

      val s = capture.result()
      if(subItemOffset > 0) {
        tokenBuffer += lexerToken(StringLiteralChunk(s.take(subItemOffset)), startLine, startCol)
      }

      val ident = s.substring(subItemOffset + 2, s.length - 1)
      tokenBuffer += lexerToken(InterpolatedIdentifier(ident), startLine, startCol + subItemOffset)

    }

    def completeState(state: StateType, endOfInput: Boolean = false) {
      import tokens._

      state match {
        case SLASH => accept(Slash, startLine, startCol)

        case AT_IDENT => {
          val ident = capture.result

          val ts = (0 until itemCount).map { n =>
            lexerToken(At, startLine, startCol + n)
          }.toVector :+ lexerToken(Identifier(ident), startLine, startCol + itemCount)

          acceptMany(ts)
        }

        case BLOCKCOMMENT => {
          if(!endOfInput) {
            val s = capture.result
            accept(BlockComment(s), startLine, startCol)
          }
          else unterminatedBlockCommentError(reader.line, reader. col)
        }

        case BLOCKCOMMENTSTAR => unterminatedBlockCommentError(reader.line, reader. col)

        case LINECOMMENT => {
          val s = capture.result
          accept(InlineComment(s), startLine, startCol)
        }

        case STRING_LIT => {
          val s = capture.result
          val t = StringLiteralChunk(s)
          if(tokenBuffer.isEmpty) {
            accept(t, startLine, startCol)
          } else {
            tokenBuffer += lexerToken(t, startLine, startCol)
            acceptMany(tokenBuffer.toVector)
          }
        }
        case LIT_ESC => unterminatedStringLiteralError(reader.line, reader. col)
        case LIT_AT => unterminatedStringLiteralError(reader.line, reader. col)
        case LIT_AT_BRACE => unterminatedStringLiteralError(reader.line, reader. col)
        case LIT_INTERP_IDENT if endOfInput => unterminatedStringLiteralError(reader.line, reader. col)
        case LIT_INTERP_IDENT => {
          //val s = capture.result
          //if

        }
      }
    }

    while(!found) {
      reader.get.map { case pc @ SourceChar(c, line, col) =>
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
            //case ':' =>   accept(Colon, line, col)
            case '-' =>   accept(Minus, line, col)
            case '+' =>   accept(Plus, line, col)
            case '*' =>   accept(Star, line, col)
            case '(' =>   accept(LParen, line, col)
            case ')' =>   accept(RParen, line, col)
            case ',' =>   accept(Comma, line, col)
            case '.' =>   accept(Dot, line, col)
            case '#' =>   accept(Hash, line, col)
            case ch if isStringLiteralDelimiter(ch) => {
              qChar = ch
              markBegin(line, col)
              itemCount = 0
              capture.clear()
              tokenBuffer.clear()
              state = STRING_LIT
            }
            case n if n.isDigit => {
              state = NUMBER
              markBegin(line, col)
              startCapture(c)
            }
            case '@' => {
              state = AT
              itemCount = 1
              markBegin(line, col)
            }
            case '/' => {
              state = SLASH
              markBegin(line, col)
            }
          }

        } else if (state == AT) {
          c match {
            case '@' =>   itemCount += 1
            case ch if isValidIdentChar(ch) => {
              startCapture(ch)
              state = AT_IDENT
            }
            case _ => error("@ must be followed by a variable name or directive", startLine, startCol)
          }

        } else if (state == AT_IDENT) {
          c match {
            case ch if isValidIdentChar(ch) => capture.append(ch)
            case _ => {
              reader.unget(pc)
              completeState(state)
            }
          }

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
              completeState(state)
            }
          }

        } else if (state == LINECOMMENT) {
          if(isLineBreak(c)) {
            completeState(state)
          } else {
            capture.append(c)
          }

        } else if (state == BLOCKCOMMENT) {
          if(c == '*') state = BLOCKCOMMENTSTAR
          else capture.append(c)

        } else if (state == BLOCKCOMMENTSTAR) {
          if(c == '/') {
            completeState(BLOCKCOMMENT)
          } else {
            capture.append('*')
            capture.append(c)
            state = BLOCKCOMMENT
          }

        } else if (state == NUMBER) {
          c match {
            case '.' => {
              capture.append('.')
              state = NUMDOT
            }
            case ch if ch.isDigit => capture.append(ch)
            case _ => {
              reader.unget(pc)
              completeState(NUMBER)
            }
          }

        } else if (state == NUMDOT) {
          if(c.isDigit) {
            capture.append(c)
            state = FLOAT
          } else {
            error("Expected digit", line, col)
          }

        } else if (state == FLOAT) {
          if (c.isDigit) {
            capture.append(c)
          } else {
            reader.unget(pc)
            completeState(FLOAT)
          }

        } else if (state == STRING_LIT) {
          if (c == qChar) {
            completeState(state)
          } else if (isLineBreak(c)) {
            unterminatedStringLiteralError(line, col)
          } else {
            capture.append(c)
            c match {
              case '@' => {
                state = LIT_AT
                subItemOffset = itemCount
              }
              case '\\' => state = LIT_ESC
            }
            itemCount += 1
          }
        } else if (state == LIT_ESC) {

          if(isLineBreak(c)) unterminatedStringLiteralError(line, col)
          else {
            capture.append(c)
            if(c == '@') {
              state = LIT_AT
              subItemOffset = itemCount
            }
            itemCount += 1
          }

        } else if ((state == LIT_AT) || (state == LIT_AT_BRACE) || (state == LIT_INTERP_IDENT)) {

          if(c == qChar) {
            completeState(STRING_LIT)
          } else if (isLineBreak(c)) {
            unterminatedStringLiteralError(line, col)
          } else if (c == '\\') {
            capture.append(c)
            itemCount += 1
            state = LIT_ESC
          } else {
            capture.append(c)
            itemCount += 1
            state match {
              case LIT_AT => if(c == '{') { state = LIT_AT_BRACE } else { state = STRING_LIT }
              case LIT_AT_BRACE => if(isValidIdentStartChar(c)) { state = LIT_INTERP_IDENT } else { state = STRING_LIT }
              case LIT_INTERP_IDENT => {
                if(c == '}') { state = LIT_INTERP_IDENT_END }
                else if (!isValidIdentChar(c)) { state = STRING_LIT }
              }
            }
          }

        } else if (state == LIT_INTERP_IDENT_END) {
          if(c == qChar) {
            completeState(state)
          } else {
            addStringLiteralChunks()
            reader.unget(pc)
            markBegin(line, col)
            itemCount = 0
            capture.clear()
            state = STRING_LIT
          }

        }

      } getOrElse {
        completeState(state, true)
      }
    }
    result
  }

  def next: Stream[Either[LexerError, LexerToken]] = {
    scan match {
      case Success(t) => {
        Right(t) #:: (new LessLexerState(reader, makeSourcePos)).next
      }
      case SuccessMany(ts) => {
        ts.foldRight((new LessLexerState(reader, makeSourcePos)).next){ Right(_) #:: _ }
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


object LessLexer extends Lexer {

  def apply(reader: Reader, makeSourcePos: (Int, Int) => Position): Stream[Either[LexerError, LexerToken]] = {
    val charReader = new CharReader(reader, 1, 1)
    (new LessLexerState(charReader, makeSourcePos)).next
  }

}