package com.kschuetz.less

import source._
import java.io.Reader
import scala.util.Try


object LessLexerState {

  object S_ extends Enumeration {
    type S_ = Value

    val START, NUMBER, NUMDOT, FLOAT, AT, AT_IDENT, SLASH,
    LINECOMMENT, BLOCKCOMMENT, BLOCKCOMMENTSTAR,
    STRING_LIT, LIT_ESC, LIT_AT, LIT_AT_BRACE, LIT_INTERP_IDENT, LIT_INTERP_IDENT_END  = Value
  }

}

private class LessLexerState(reader: CharReader, makeSourcePos: (Int, Int) => Position) {

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


  object State {
    import tokens._

    var done = false
    var result: TokenResult = NothingLeft
    var startLine: Int = 0
    var startCol: Int = 0
    var itemCount: Int = 0
    var subItemOffset: Int = 0;
    var qChar: Char = '\0'
    var handler: (Option[SourceChar] => Unit) = top

    var capture: StringBuilder = null

    val tokenBuffer = new collection.mutable.ArrayBuffer[LexerToken]

    def accept(token: Token, line: Int, col: Int): Unit = {
      result = Success(LexerToken(token, makeSourcePos(line, col)))
      done = true
    }

    def acceptMany(tokens: Vector[LexerToken]): Unit = {
      result = SuccessMany(tokens)
      done = true
    }

    def error(msg: String, line: Int, col: Int): Unit = {
      val e = ParseError(msg, makeSourcePos(line, col))
      result = Failure(e)
      done = true
    }

    def error(msg: String, sourceChar: Option[SourceChar]): Unit = {
      val (line, col) = toLineCol(sourceChar)
      val e = ParseError(msg, makeSourcePos(line, col))
      result = Failure(e)
      done = true
    }

    /**
     * Extracts line and col from a Some(SourceChar), or
     * from the reader position if None (which will point to the
     * position at the end of input)
     */
    def toLineCol(sc: Option[SourceChar]): (Int, Int) = {
      sc.map { x => (x.line, x.col) } getOrElse { (reader.line, reader.col) }
    }

    def unterminatedBlockCommentError(sourceChar: Option[SourceChar]): Unit =
      error("Unterminated block comment", sourceChar)


    def unterminatedStringLiteralError(sourceChar: Option[SourceChar]): Unit =
      error("Unterminated string literal", sourceChar)

    def markBegin(line: Int, col: Int) = {
      startLine = line
      startCol = col
    }

    def resetCapture(): Unit = {
      if(capture==null) { capture = new StringBuilder }
      else { capture.clear() }
    }

    def addStringLiteralChunks(): Unit = {
      import tokens._

      val s = capture.result()
      if(subItemOffset > 0) {
        tokenBuffer += lexerToken(StringLiteralChunk(s.take(subItemOffset)), startLine, startCol)
      }

      val ident = s.substring(subItemOffset + 2, s.length - 1)
      tokenBuffer += lexerToken(InterpolatedIdentifier(ident), startLine, startCol + subItemOffset)

    }

    // ***** handlers *****

    def at1(input: Option[SourceChar]): Unit = {
      var ok = input.isDefined
      input.foreach { sc =>
        if(sc.c == '@') { itemCount += 1 }
        else if (isValidIdentChar(sc.c)) {
          resetCapture()
          capture.append(sc.c)
          handler = atIdent
        } else ok = false
      }
      if(!ok) error("@ must be followed by a variable name or directive", startLine, startCol)
    }

    def atIdent(input: Option[SourceChar]): Unit = {
      var more = false
      input.foreach { sc =>
        if(isValidIdentChar(sc.c)) { more = true; capture.append(sc.c)}
        else { reader.unget(sc) }
      }
      if(!more) {
        val ident = capture.result
        val ts = (0 until itemCount).map { n =>
          lexerToken(At, startLine, startCol + n)
        }.toVector :+ lexerToken(Identifier(ident), startLine, startCol + itemCount)
        acceptMany(ts)
      }
    }

    def lineComment(input: Option[SourceChar]): Unit = {
      var more = false
      input.foreach { sc =>
        if(!isLineBreak(sc.c)) { more = true; capture.append(sc.c) }
      }
      if(!more) {
        val s = capture.result
        accept(InlineComment(s), startLine, startCol)
      }
    }

    def blockComment(input: Option[SourceChar]): Unit = {
      input.map { sc =>
        if(sc.c == '*') handler = blockCommentStar
        else capture.append(sc.c)
      } getOrElse {
        unterminatedBlockCommentError(input)
      }
    }

    def blockCommentStar(input: Option[SourceChar]): Unit = {
      input.map { sc =>
        if(sc.c == '/') {
          val s = capture.result
          accept(BlockComment(s), startLine, startCol)
        } else {
          capture.append('*')
          capture.append(sc.c)
          handler = blockComment
        }
      } getOrElse {
        unterminatedBlockCommentError(input)
      }
    }

    def slash(input: Option[SourceChar]): Unit = {
      var more = false
      input.foreach { sc =>
        sc.c match {
          case '/' => { resetCapture(); more = true; handler = lineComment }
          case '*' => { resetCapture(); more = true; handler = blockComment }
          case _ => { reader.unget(sc) }
        }
      }
      if(!more) accept(Slash, startLine, startCol)
    }


    def number(input: Option[SourceChar]): Unit = {
      var more = false
      input.foreach { sc =>
        if(sc.c.isDigit) { more = true; capture.append(sc.c) }
        else if(sc.c == '.') { more = true; capture.append('.'); handler = numberDot }
        else reader.unget(sc)
      }
      if(!more) {
        val s = capture.result
        accept(WholeNumber(BigInt(s)), startLine, startCol)
      }
    }

    def numberDot(input: Option[SourceChar]): Unit = {
      var ok = false
      input.foreach { sc =>
        if(sc.c.isDigit) { ok = true; capture.append(sc.c); handler = numberFloat }
      }
      if(!ok) error("Expected digit", input)
    }

    def numberFloat(input: Option[SourceChar]): Unit = {
      var more = false
      input.foreach { sc =>
        if(sc.c.isDigit) { more = true; capture.append(sc.c) }
        else reader.unget(sc)
      }
      if(!more) {
        val s = capture.result()
        Try(s.toDouble).toOption match {
          case Some(d) => accept(FloatNumber(d), startLine, startCol)
          case _ => error("Could not parse floating point number", startLine, startCol)
        }
      }
    }

    def stringLiteral(input: Option[SourceChar]): Unit = {
      var ok = false
      input.foreach { sc =>
        if(sc.c == qChar) {
          val s = capture.result
          val t = StringLiteralChunk(s)
          if(tokenBuffer.isEmpty) {
            accept(t, startLine, startCol)
          } else {
            tokenBuffer += lexerToken(t, startLine, startCol)
            acceptMany(tokenBuffer.toVector)
          }

          ok = true
        } else if (!isLineBreak(sc.c)) {
          capture.append(sc.c)
          sc.c match {
            case '@' => { subItemOffset = itemCount; handler = stringLiteralAt }
            case '\\' => { handler = stringLiteralEsc }
          }
          itemCount += 1
          ok = true
        }
      }
      if(!ok) unterminatedStringLiteralError(input)
    }

    def stringLiteralEsc(input: Option[SourceChar]): Unit = {
      input.map { sc =>
        if(sc.c == qChar) { capture.append(sc.c); itemCount += 1; handler = stringLiteral }
        else { reader.unget(sc); handler = stringLiteral }
      } getOrElse {
        unterminatedStringLiteralError(None)
      }
    }

    def stringLiteralAt(input: Option[SourceChar]): Unit = {
      input.map { sc =>
        if(sc.c == '{') { capture.append(sc.c); itemCount += 1; handler = stringLiteralAtBrace }
        else { reader.unget(sc); handler = stringLiteral }
      } getOrElse {
        unterminatedStringLiteralError(None)
      }
    }

    def stringLiteralAtBrace(input: Option[SourceChar]): Unit = {
      input.map { sc =>
        if(isValidIdentStartChar(sc.c)) { capture.append(sc.c); itemCount += 1; handler = stringLiteralInterpolate }
        else { reader.unget(sc); handler = stringLiteral }
      } getOrElse {
        unterminatedStringLiteralError(None)
      }
    }

    def stringLiteralInterpolate(input: Option[SourceChar]): Unit = {
      input.map { sc =>
        if(sc.c == '}') { handler = stringLiteralInterpolateEnd }
        if(isValidIdentChar(sc.c)) { capture.append(sc.c); itemCount += 1 }
        else { reader.unget(sc); handler = stringLiteral }
      } getOrElse {
        unterminatedStringLiteralError(None)
      }
    }

    def stringLiteralInterpolateEnd(input: Option[SourceChar]): Unit = {
      input.map { case sc @ SourceChar(c, line, col) =>
        addStringLiteralChunks()
        if(c == qChar) {
          // the closing quote immediately succeeded to closing brace
          acceptMany(tokenBuffer.toVector)

        } else {
          // there is more literal text to follow the closing brace
          reader.unget(sc)
          markBegin(line, col)
          itemCount = 0
          resetCapture()
          handler = stringLiteral
        }
      } getOrElse {
        unterminatedStringLiteralError(None)
      }
    }


    def top(input: Option[SourceChar]): Unit = {
      input.foreach { case sc @ SourceChar(c, line, col) =>
        c match {
          case '@' => { itemCount = 1; markBegin(line, col); handler = at1 }
          case '/' => { markBegin(line, col); handler = slash }
          case n if n.isDigit => { markBegin(line, col); resetCapture(); capture.append(n); handler = number }
          case ch if isStringLiteralDelimiter(ch) => {
            qChar = ch; markBegin(line, col); itemCount = 0; resetCapture(); tokenBuffer.clear(); handler = stringLiteral
          }
        }
      }
    }

  }




  def scan: TokenResult = {
    import State._
    while(!done) {
      handler(reader.get)
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