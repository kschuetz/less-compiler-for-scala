package com.kschuetz.less

import source._
import java.io.Reader
import scala.util.Try
import scala.util.parsing.input.Position


private class LessLexerState(reader: CharReader,
                             makeSourcePos: (Int, Int) => Position,
                             prevToken: Option[TokenValue]) {

  sealed abstract trait TokenResult
  case class Success(token: Token) extends TokenResult
  case class SuccessMany(tokens: Vector[Token]) extends TokenResult
  case class Failure(error: Token) extends TokenResult
  case object NothingLeft extends TokenResult


  def isValidIdentChar(c: Char) = {
    c.isLetterOrDigit || (c == '-') || (c == '_')
  }

  def isValidVarNameStartChar(c: Char) =
    isValidIdentChar(c)

  def isValidIdentStartChar(c: Char) = {
    c.isLetter || (c == '-')
  }

  def isValidHashIdentChar(c: Char) = {
    isValidIdentChar(c)
  }

  def isValidPseudoClassStartChar(c: Char) = {
    c.isLetterOrDigit || (c == '-') || (c == '_') || (c ==':')
  }

  def isLineBreak(c: Char) =
    (c == '\r') || (c == '\n')

  def isStringLiteralDelimiter(c: Char) =
    (c == '\'') || (c == '"')

  def isNonPrintable(c: Char) = {
    val n = c.toInt
    (n >= 0 && n <= 8) || (n == 11) || (n >= 14 && n <= 31) || (n == 127)
  }


  /**
   * Extracts line and col from a Some(SourceChar), or
   * from the reader position if None (which will point to the
   * position at the end of input)
   */
  def toLineCol(sc: Option[SourceChar]): (Int, Int) = {
    sc.map { x => (x.line, x.col) } getOrElse { (reader.line, reader.col) }
  }


  object State {
    import tokens._

    var done = false
    var result: TokenResult = NothingLeft
    var startLine: Int = 0
    var startCol: Int = 0
    var itemCount: Int = 0
    var subState: Int = 0     // context sensitive
    var subItemOffset: Int = 0;
    var qChar: Char = '\0'
    var handler: (Option[SourceChar] => Unit) = top
    
    var whitespaceEncountered: Boolean = false
    var newLineEncountered: Boolean = false

    var capture: StringBuilder = null
    var tokenBuffer: collection.mutable.ArrayBuffer[Token] = null


    def makeToken(token: TokenValue, line: Int, col: Int, firstInSequence: Boolean = true): Token = {
      val followsWhitespace = firstInSequence && (whitespaceEncountered || newLineEncountered ||
        prevToken.map(_.isComment).getOrElse(false))
      val immediatelyFollowsComment = firstInSequence && (!(whitespaceEncountered || newLineEncountered) && (prevToken.map(_.isComment).getOrElse(false)))

      Token(token,
        TokenContext(makeSourcePos(line, col),
        followsWhitespace,
        newLineEncountered && firstInSequence,
        immediatelyFollowsComment))
    }

    def makeToken(token: TokenValue, sourceChar: Option[SourceChar], firstInSequence: Boolean): Token = {
      val (line, col) = toLineCol(sourceChar)
      makeToken(token, line, col, firstInSequence)
    }

    def accept(token: TokenValue, line: Int, col: Int): Unit = {
      result = Success(makeToken(token, line, col))
      done = true
    }

    def accept(token: TokenValue, sourceChar: Option[SourceChar]): Unit = {
      val (line, col) = toLineCol(sourceChar)
      result = Success(makeToken(token, line, col))
      done = true
    }

    def acceptMany(tokens: Vector[Token]): Unit = {
      result = SuccessMany(tokens)
      done = true
    }

    def error(msg: String, line: Int, col: Int): Unit = {
      val e = LexerError(msg)
      result = Failure(makeToken(e, line, col))
      done = true
    }

    def error(msg: String, sourceChar: Option[SourceChar]): Unit = {
      val (line, col) = toLineCol(sourceChar)
      val e = LexerError(msg)
      result = Failure(makeToken(e, line, col))
      done = true
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
      if(capture == null) { capture = new StringBuilder }
      else { capture.clear() }
    }

    def resetTokenBuffer(): Unit = {
      if(tokenBuffer == null) { tokenBuffer = new collection.mutable.ArrayBuffer[Token] }
      else { tokenBuffer.clear() }
    }

    def addStringLiteralChunks(): Unit = {
      import tokens._

      val s = capture.result()
      if(subItemOffset > 0) {
        tokenBuffer += makeToken(StringLiteralChunk(s.take(subItemOffset)), startLine, startCol, false)
      }

      val ident = s.substring(subItemOffset + 2, s.length)
      tokenBuffer += makeToken(InterpolatedIdentifier(ident), startLine, startCol + subItemOffset, false)

    }

    // ***** handlers *****


    def at1(input: Option[SourceChar]): Unit = {
      var ok = input.isDefined
      input.foreach { sc =>
        if (isValidVarNameStartChar(sc.c)) {
          resetCapture()
          capture.append(sc.c)
          if(subState == 2) { subState = 0; handler = atBraceIdent }
          else handler = atIdent
        }
        else if((subState == 0) && (sc.c == '@')) subState = 1
        else if((subState == 0) && (sc.c == '{')) subState = 2
        else ok = false
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
        if(subState == 1) accept(AtAtIdentifier(ident), startLine, startCol)
        else accept(AtIdentifier(ident), startLine, startCol)
      }
    }

    def atBraceIdent(input: Option[SourceChar]): Unit = {
      if(!input.isDefined) {
        error("Unterminated @{} expression", input)
      } else {
        var ok = false
        var more = false
        input.foreach { sc =>
          if(isValidIdentChar(sc.c)) { more = true; ok = true; capture.append(sc.c)}
          else if (sc.c == '}') { ok = true }
          else error("Illegal character in identifier", input)
        }
        if(ok && !more) {
          val ident = capture.result
          if(subState == 1) accept(DotAtBraceIdentifier(ident), startLine, startCol)
          else accept(AtBraceIdentifier(ident), startLine, startCol)
        }
      }
    }

    def dot1(input: Option[SourceChar]): Unit = {
      var ok = input.isDefined
      input.foreach { sc =>
        if(sc.c.isDigit) {
          resetCapture()
          capture.append('.')
          capture.append(sc.c)
          handler = numberFloat
        } else if(sc.c == '.') {
          handler = dot2
        } else if(isValidIdentStartChar(sc.c)) {
          resetCapture()
          capture.append(sc.c)
          handler = dotIdent
        } else if(sc.c == '@') handler = dotAt
          else ok = false
      }
      if(!ok) error(". must be followed by an identifier", startLine, startCol)
    }

    def dot2(input: Option[SourceChar]): Unit = {
      var ok = input.isDefined
      input.foreach { sc =>
        if(sc.c == '.') {
          accept(DotDotDot, startLine, startCol)
        } else ok = false
      }
      if(!ok) error("Unrecognized operator (..)", startLine, startCol)
    }

    def dotIdent(input: Option[SourceChar]): Unit = {
      var more = false
      input.foreach { sc =>
        if(isValidIdentChar(sc.c)) { more = true; capture.append(sc.c)}
        else { reader.unget(sc) }
      }
      if(!more) {
        val ident = capture.result
        accept(DotIdentifier(ident), startLine, startCol)
      }
    }

    def dotAt(input: Option[SourceChar]): Unit = {
      var ok = input.isDefined
      input.foreach { sc =>
        if(sc.c == '{') {
          ok = true
          resetCapture()
          subState = 1
          handler = atBraceIdent
        }
      }
      if(!ok) error("Expected {", startLine, startCol)
    }

    def hash1(input: Option[SourceChar]): Unit = {
      var ok = input.isDefined
      input.foreach { sc =>
        if(isValidHashIdentChar(sc.c)) {
          resetCapture()
          capture.append(sc.c)
          handler = hashIdent
        } else ok = false
      }
      if(!ok) error("# must be followed by an identifier", startLine, startCol)
    }

    def hashIdent(input: Option[SourceChar]): Unit = {
      var more = false
      input.foreach { sc =>
        if(isValidHashIdentChar(sc.c)) { more = true; capture.append(sc.c)}
        else { reader.unget(sc) }
      }
      if(!more) {
        val ident = capture.result
        accept(HashIdentifier(ident), startLine, startCol)
      }
    }

    def backslash1(input: Option[SourceChar]): Unit = {
      var ok = input.isDefined
      input.foreach { sc =>
        if(sc.c.isDigit) {
          reader.unget(sc)
          accept(Backslash, startLine, startCol)
        } else ok = false
      }
      if(!ok) error("\\ must be followed by a digit", startLine, startCol)
    }

    def eq1(input: Option[SourceChar]): Unit = {
      var found = false
      input.foreach { sc =>
        if(sc.c == '<') { found = true; accept(LtEq, startLine, startCol) }
        else if(sc.c == '>') { found = true; accept(GtEq, startLine, startCol) }
        else {
          reader.unget(sc)
        }
      }
      if(!found) accept(Eq, startLine, startCol)
    }

    def gt1(input: Option[SourceChar]): Unit = {
      var found = false
      input.foreach { sc =>
        if(sc.c == '=') { found = true; accept(GtEq, startLine, startCol) }
        else { reader.unget(sc) }
      }
      if(!found) accept(Gt, startLine, startCol)
    }

    def lt1(input: Option[SourceChar]): Unit = {
      var found = false
      input.foreach { sc =>
        if(sc.c == '=') { found = true; accept(LtEq, startLine, startCol) }
        else { reader.unget(sc) }
      }
      if(!found) accept(Lt, startLine, startCol)
    }

    def matchOp(required: Boolean, toToken: TokenValue, input: Option[SourceChar]): Boolean = {
      var found = false
      input.foreach { sc =>
        if(sc.c == '=') { found = true; accept(toToken, startLine, startCol) }
        else if (!required) { reader.unget(sc) }
      }
      if(!found && required) error("Illegal character", startLine, startCol)
      found
    }

    def tilde1(input: Option[SourceChar]): Unit = {
      var found = false
      input.map { sc =>
        if(isStringLiteralDelimiter(sc.c)) {
          found = true
          prepareForStringLiteral(sc.c, true)
          handler = stringLiteral
        }
      }
      if(!found) matchOp(true, Includes, input)
    }

    def pipe1(input: Option[SourceChar]): Unit =
      matchOp(true, DashMatch, input)

    def caret1(input: Option[SourceChar]): Unit =
      matchOp(true, PrefixMatch, input)

    def dollar1(input: Option[SourceChar]): Unit =
      matchOp(true, SuffixMatch, input)

    def star1(input: Option[SourceChar]): Unit = {
      if(!matchOp(false, SubstringMatch, input)) {
        accept(Star, startLine, startCol)
      }
    }

    def lineComment(input: Option[SourceChar]): Unit = {
      var more = false
      input.foreach { sc =>
        if(!isLineBreak(sc.c)) { more = true; capture.append(sc.c) }
        else { reader.unget(sc) }
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
        var discard = false

        if(sc.c.isDigit) { more = true; capture.append(sc.c) }
        else if(sc.c == '.') { more = true; capture.append('.'); handler = numberDot }
        else if(sc.c == 'e' || sc.c == 'E') {
          discard = true
          reader.get.map { c1 =>
            if(c1.c == '+' || c1.c == '-') reader.get.map { c2 =>
              if(c2.c.isDigit) {
                // 1e+2
                more = true
                subState = 0
                handler = numberFloat
              }
              reader.unget(c2)
            } else if (c1.c.isDigit) {
              // 1e2
              more = true
              subState = 0
              handler = numberFloat
            }
            reader.unget(c1)
          }

        } else discard = true

        if(discard) reader.unget(sc)
      }
      if(!more) {
        val s = capture.result
        accept(WholeNumber(BigInt(s)), startLine, startCol)
      }
    }

    def numberDot(input: Option[SourceChar]): Unit = {
      var ok = false
      input.foreach { sc =>
        if(sc.c.isDigit) { ok = true; capture.append(sc.c); subState = 0; handler = numberFloat }
      }
      if(!ok) error("Expected digit", input)
    }

    def numberFloat(input: Option[SourceChar]): Unit = {
      var more = false
      input.foreach { sc =>
        var keep = false
        if(sc.c.isDigit) { more = true; keep = true; capture.append(sc.c) }
        else if(subState == 0 && (sc.c == 'e' || sc.c == 'E')) {
          reader.get.map { c1 =>
            if(c1.c == '+' || c1.c == '-') reader.get.map { c2 =>
              if(c2.c.isDigit) {
                subState = 1
                capture.append(sc.c)
                capture.append(c1.c)
                capture.append(c2.c)
                more = true
                keep = true
              } else {
                reader.unget(c2)
                reader.unget(c1)
              }
            } else {
              reader.unget(c1)
            }
          }
        }
        if(!keep) reader.unget(sc)
      }
      if(!more) {
        val s = capture.result()
        Try(s.toDouble).toOption match {
          case Some(d) => accept(RealNumber(s, d), startLine, startCol)
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

          tokenBuffer += makeToken(t, startLine, startCol, false)
          tokenBuffer += makeToken(StringLiteralEnd, sc.line, sc.col, false)
          acceptMany(tokenBuffer.toVector)

          ok = true
        } else if (!isLineBreak(sc.c)) {
          capture.append(sc.c)
          if(sc.c == '@') { subItemOffset = itemCount; handler = stringLiteralAt }
          else if (sc.c == '\\') { handler = stringLiteralEsc }
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
        if(isValidVarNameStartChar(sc.c)) { capture.append(sc.c); itemCount += 1; handler = stringLiteralInterpolate }
        else { reader.unget(sc); handler = stringLiteral }
      } getOrElse {
        unterminatedStringLiteralError(None)
      }
    }

    def stringLiteralInterpolate(input: Option[SourceChar]): Unit = {
      input.map { sc =>
        if(sc.c == '}') { handler = stringLiteralInterpolateEnd }
        else if(isValidIdentChar(sc.c)) { capture.append(sc.c); itemCount += 1 }
        else { reader.unget(sc); handler = stringLiteral }
      } getOrElse {
        unterminatedStringLiteralError(None)
      }
    }

    def stringLiteralInterpolateEnd(input: Option[SourceChar]): Unit = {
      input.map { case sc @ SourceChar(c, line, col) =>
        addStringLiteralChunks()
        if(c == qChar) {
          // the closing quote immediately followed the closing brace

          tokenBuffer += makeToken(StringLiteralEnd, sc.line, sc.col, false)
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

    def identifier(input: Option[SourceChar]): Unit = {
      var more = false
      input.foreach { sc =>
        if(isValidIdentChar(sc.c)) { more = true; capture.append(sc.c) }
        else reader.unget(sc)
      }
      if(!more) {
        val s = capture.result
        accept(Identifier(s), startLine, startCol)
      }
    }

    def urlSymbol(input: Option[SourceChar]): Unit = {
      if(!input.isDefined) {
        identifier(input)
      } else input.foreach { sc =>
          if(subState == 0 && (sc.c == 'r')) { capture.append(sc.c); subState = 1 }
          else if (subState == 1 && (sc.c == 'l')) { capture.append(sc.c); subState = 2 }
          else if (subState == 2 && (sc.c.isWhitespace)) { /* do nothing */ }
          else if (subState == 2 && (sc.c == '(')) { resetCapture(); subState = 0; handler = urlValue1 }
          else { reader.unget(sc); handler = identifier }
      }
    }

    def urlValue1(input: Option[SourceChar]): Unit = {
      if(!input.isDefined) {
        accept(Url, startLine, startCol)
      } else input.foreach { sc =>
        var beginUnquoted = false
        if(isStringLiteralDelimiter(sc.c)) {
          reader.unget(sc)
          accept(Url, startLine, startCol)
        } else if (sc.c == '~') {
          beginUnquoted = true
          reader.get.foreach { c2 =>
            if(isStringLiteralDelimiter(c2.c)) {
              reader.unget(c2)
              reader.unget(sc)
              beginUnquoted = false
              accept(Url, startLine, startCol)
            }
          }
        } else if (sc.c == ')') {
          val tokens = Vector(makeToken(Url, startLine, startCol, true),
                              makeToken(RParen, input, false))
          acceptMany(tokens)
        } else if(sc.c.isWhitespace) {
          /* do nothing */  
        } else {
          beginUnquoted = true
        }

        if(beginUnquoted) {
          // unquoted string in URL
          resetTokenBuffer()
          tokenBuffer += makeToken(Url, startLine, startCol, true)
          reader.unget(sc)
          resetCapture()
          markBegin(sc.line, sc.col)
          handler = urlUnquoted
        }

      }
    }

    def urlUnquoted(input: Option[SourceChar]): Unit = {
      var ok = true
      var more = false
      input.foreach { sc =>
        sc.c match {
          case ')' => { reader.unget(sc) }
          case ch if ch.isWhitespace =>  { reader.unget(sc) }
          case '\\' => {
            capture.append('\\')
            reader.get.map { c2 =>
              if(c2.c == ')') { reader.unget(c2) }
              else { capture.append(c2.c); more = true }
            }
          }
          case '(' => ok = false
          case '"' => ok = false
          case '\'' => ok = false
          case ch if isNonPrintable(ch) => ok = false
          case ch => {
            capture.append(ch)
            more = true
          }
        }
      }
      if(!ok) error("Illegal character in unquoted URL", input)
      else if (!more) {
        val s = capture.result
        tokenBuffer += makeToken(UnquotedString(s), startLine, startCol, false)
        acceptMany(tokenBuffer.toVector)
      }
    }

    def dash1(input: Option[SourceChar]): Unit = {
      var more = false
      input.foreach { sc =>
        if(sc.c.isLetter) { more = true; resetCapture(); capture.append('-'); capture.append(sc.c); handler = identifier }
        else reader.unget(sc)
      }
      if(!more) accept(Minus, startLine, startCol)
    }

    def prepareForStringLiteral(delimiter: Char, precededByTilde: Boolean): Unit = {
      qChar = delimiter
      itemCount = 0
      resetCapture()
      resetTokenBuffer()
      val openToken =
        if(precededByTilde) TildeQuoteLiteral
        else if(delimiter == '"') DoubleQuoteLiteral
        else SingleQuoteLiteral

      tokenBuffer.append(makeToken(openToken, startLine, startCol))
    }

    def top(input: Option[SourceChar]): Unit = {
      input.map { case sc @ SourceChar(c, line, col) =>
        c match {
          case 'u' => { markBegin(line, col); resetCapture(); capture.append(c); subState = 0; handler = urlSymbol }
          case ch if isValidIdentStartChar(c) => { markBegin(line, col); resetCapture(); capture.append(ch); handler = identifier }
          case '@' => { markBegin(line, col); subState = 0; handler = at1 }
          case '-' => { markBegin(line, col); handler = dash1 }
          case '.' => { markBegin(line, col); handler = dot1 }
          case ';' => { accept(Semicolon, line, col) }
          case ',' => { accept(Comma, line, col)}
          case '{' => { accept(LBrace, line, col) }
          case '(' => { accept(LParen, line, col) }
          case '[' => { accept(LBracket, line, col) }
          case '}' => { accept(RBrace, line, col) }
          case ')' => { accept(RParen, line, col) }
          case ']' => { accept(RBracket, line, col) }
          case ':' => { accept(Colon, line, col) }
          case '/' => { markBegin(line, col); handler = slash }
          case '#' => { markBegin(line, col); handler = hash1 }
          case '>' => { markBegin(line, col); handler = gt1 }
          case '<' => { markBegin(line, col); handler = lt1 }
          case '=' => { markBegin(line, col); handler = eq1 }
          case '*' => { markBegin(line, col); handler = star1 }
          case '|' => { markBegin(line, col); handler = pipe1 }
          case '^' => { markBegin(line, col); handler = caret1 }
          case '~' => { markBegin(line, col); handler = tilde1 }
          case '$' => { markBegin(line, col); handler = dollar1 }
          case '+' => { accept(Plus, line, col) }
          case '!' => { accept(Bang, line, col) }
          case '%' => { accept(Percent, line, col) }
          case '&' => { accept(Ampersand, line, col)}
          case '\\' => { markBegin(line, col); handler = backslash1 }

          case n if n.isDigit => { markBegin(line, col); resetCapture(); capture.append(n); handler = number }
          case ch if isStringLiteralDelimiter(ch) => {
            markBegin(line, col)
            prepareForStringLiteral(ch, false)
            handler = stringLiteral
          }

          case ch if isLineBreak(ch) => { newLineEncountered = true; whitespaceEncountered = true }
          case ch if ch.isWhitespace => { whitespaceEncountered = true }
          case ch => { accept(Unknown(ch.toString), line, col)}
        }
      } getOrElse {

        result = NothingLeft
        done = true

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


  def next: Stream[Token] = {
    scan match {
      case Success(t) => {
        t #:: (new LessLexerState(reader, makeSourcePos, Some(t.value))).next
      }
      case SuccessMany(ts) => {
        val lastToken = ts.lastOption.map { _.value }
        val nextState = (new LessLexerState(reader, makeSourcePos, lastToken)).next
        ts.foldRight(nextState){ _ #:: _ }
      }
      case Failure(e) => {
        reader.close()
        e #:: Stream.empty
      }
      case NothingLeft => {
        reader.close()
        Stream.empty
      }
    }
  }
}


object LessLexer extends Lexer {

  def apply(reader: Reader, makeSourcePos: (Int, Int) => Position): Stream[Token] = {
    val charReader = new CharReader(reader, 1, 1) with Markable
    (new LessLexerState(charReader, makeSourcePos, None)).next
  }

}