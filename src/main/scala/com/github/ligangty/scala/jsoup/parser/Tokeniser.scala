package com.github.ligangty.scala.jsoup.parser

import com.github.ligangty.scala.jsoup.helper.Validator._
import com.github.ligangty.scala.jsoup.nodes.Entities

/**
 * Readers the input stream into tokens.
 */
final private[parser] class Tokeniser {
  private var reader: CharacterReader = null
  private var errors: ParseErrorList = null
  private var state: TokeniserState = Data
  private var emitPending: Token = null
  private var isEmitPending: Boolean = false
  private var charsString: String = null
  private var charsBuilder: StringBuilder = new StringBuilder(1024)
  private[parser] var dataBuffer: StringBuilder = new StringBuilder(1024)
  private[parser] var tagPending: Token.Tag = null
  private[parser] var startPending: Token.StartTag = new Token.StartTag()
  private[parser] var endPending: Token.EndTag = new Token.EndTag
  private[parser] var charPending: Token.Character = new Token.Character
  private[parser] var doctypePending: Token.Doctype = new Token.Doctype
  private[parser] var commentPending: Token.Comment = new Token.Comment
  private var lastStartTag: String = null
  private var selfClosingFlagAcknowledged: Boolean = true

  private[parser] def this(reader: CharacterReader, errors: ParseErrorList) {
    this()
    this.reader = reader
    this.errors = errors
  }

  private[parser] def read: Token = {
    if (!selfClosingFlagAcknowledged) {
      error("Self closing flag not acknowledged")
      selfClosingFlagAcknowledged = true
    }
    while (!isEmitPending) state.read(this, reader)
    // if emit is pending, a non-character token was found: return any chars in buffer, and leave token for next read:
    if (charsBuilder.length > 0) {
      val str: String = charsBuilder.toString()
      charsBuilder.delete(0, charsBuilder.length)
      charsString = null
      charPending.data(str)
    } else if (charsString != null) {
      val token: Token = charPending.data(charsString)
      charsString = null
      token
    } else {
      isEmitPending = false
      emitPending
    }
  }

  private[parser] def emit(token: Token) {
    isFalse(isEmitPending, "There is an unread token pending!")
    emitPending = token
    isEmitPending = true
    if (token.tokType == Token.TokenType.StartTag) {
      val startTag: Token.StartTag = token.asInstanceOf[Token.StartTag]
      lastStartTag = startTag.tagName
      if (startTag.selfClosing) selfClosingFlagAcknowledged = false
    }
    else if (token.tokType == Token.TokenType.EndTag) {
      val endTag: Token.EndTag = token.asInstanceOf[Token.EndTag]
      if (endTag.attributes != null) error("Attributes incorrectly present on end tag")
    }
  }

  private[parser] def emit(str: String) {
    // buffer strings up until last string token found, to emit only one token for a run of character refs etc.
    // does not set isEmitPending; read checks that
    if (charsString == null) {
      charsString = str
    } else {
      if (charsBuilder.length == 0) {
        // switching to string builder as more than one emit before read
        charsBuilder.append(charsString)
      }
      charsBuilder.append(str)
    }
  }

  private[parser] def emit(chars: Array[Char]) {
    emit(String.valueOf(chars))
  }

  private[parser] def emit(c: Char) {
    emit(String.valueOf(c))
  }

  private[parser] def getState: TokeniserState = state

  private[parser] def transition(state: TokeniserState) {
    this.state = state
  }

  private[parser] def advanceTransition(state: TokeniserState) {
    reader.advance()
    this.state = state
  }

  private[parser] def acknowledgeSelfClosingFlag(): Unit = {
    selfClosingFlagAcknowledged = true
  }

  // holder to not have to keep creating arrays
  private val charRefHolder: Array[Char] = new Array[Char](1)

  private[parser] def consumeCharacterReference(additionalAllowedCharacter: Option[Char], inAttribute: Boolean): Array[Char] = {
    if (reader.isEmpty) return null
    additionalAllowedCharacter match {
      case Some(char) if char == reader.current => return null
    }
//    if (additionalAllowedCharacter != null && additionalAllowedCharacter == reader.current) return null
    if (reader.matchesAnySorted(Tokeniser.notCharRefCharsSorted)) return null
    val charRef: Array[Char] = charRefHolder
    reader.mark()
    if (reader.matchConsume("#")) {
      // numbered
      val isHexMode: Boolean = reader.matchConsumeIgnoreCase("X")
      val numRef: String = if (isHexMode) reader.consumeHexSequence else reader.consumeDigitSequence
      if (numRef.length == 0) {
        // didn't match anything
        characterReferenceError("numeric reference with no numerals")
        reader.rewindToMark()
        return null
      }
      // missing semi
      if (!reader.matchConsume(";")) characterReferenceError("missing semicolon")
      var charval: Int = -1
      try {
        val base: Int = if (isHexMode) 16 else 10
        charval = Integer.valueOf(numRef, base)
      } catch {
        // skip
        case e: NumberFormatException => ()
      }
      if (charval == -1 || (charval >= 0xD800 && charval <= 0xDFFF) || charval > 0x10FFFF) {
        characterReferenceError("character outside of valid range")
        charRef(0) = Tokeniser.replacementChar
        charRef
      } else {
        // todo: implement number replacement table
        // todo: check for extra illegal unicode points as parse errors
        if (charval < Character.MIN_SUPPLEMENTARY_CODE_POINT) {
          charRef(0) = charval.toChar
          charRef
        } else Character.toChars(charval)
      }
    } else {
      // named
      // get as many letters as possible, and look for matching entities.
      val nameRef: String = reader.consumeLetterThenDigitSequence
      val looksLegit: Boolean = reader.matches(';')
      // found if a base named entity without a ;, or an extended entity with the ;.
      val found: Boolean = Entities.isBaseNamedEntity(nameRef) || (Entities.isNamedEntity(nameRef) && looksLegit)
      if (!found) {
        reader.rewindToMark()
        // named with semicolon
        if (looksLegit) characterReferenceError(String.format("invalid named referenece '%s'", nameRef))
        return null
      }
      if (inAttribute && (reader.matchesLetter || reader.matchesDigit || reader.matchesAny('=', '-', '_'))) {
        reader.rewindToMark()
        return null
      }
      // missing semi
      if (!reader.matchConsume(";")) characterReferenceError("missing semicolon")
      charRef(0) = Entities.getCharacterByName(nameRef)
      charRef
    }
  }

  private[parser] def createTagPending(start: Boolean): Token.Tag = {
    tagPending = if (start) startPending.reset else endPending.reset
    tagPending
  }

  private[parser] def emitTagPending(): Unit = {
    tagPending.finaliseTag()
    emit(tagPending)
  }

  private[parser] def createCommentPending(): Unit = {
    commentPending.reset
  }

  private[parser] def emitCommentPending(): Unit = {
    emit(commentPending)
  }

  private[parser] def createDoctypePending(): Unit = {
    doctypePending.reset
  }

  private[parser] def emitDoctypePending(): Unit = {
    emit(doctypePending)
  }

  private[parser] def createTempBuffer(): Unit = {
    Token.reset(dataBuffer)
  }

  private[parser] def isAppropriateEndTagToken: Boolean = {
    lastStartTag != null && (tagPending.tagName == lastStartTag)
  }

  private[parser] def appropriateEndTagName: String = {
    if (lastStartTag == null) return null
    lastStartTag
  }

  private[parser] def error(state: TokeniserState) {
    if (errors.canAddError) errors.append(new ParseError(reader.pos, "Unexpected character '%s' in input state [%s]", reader.current, state))
  }

  private[parser] def eofError(state: TokeniserState) {
    if (errors.canAddError) errors.append(new ParseError(reader.pos, "Unexpectedly reached end of file (EOF) in input state [%s]", state))
  }

  private def characterReferenceError(message: String) {
    if (errors.canAddError) errors.append(new ParseError(reader.pos, "Invalid character reference: %s", message))
  }

  private def error(errorMsg: String) {
    if (errors.canAddError) errors.append(new ParseError(reader.pos, errorMsg))
  }

  private[parser] def currentNodeInHtmlNS: Boolean = {
    // todo: implement namespaces correctly
    true
    // val curNode = currentNode()
    // (curNode != null) && curNode.namespace().equals("HTML")
  }

  /**
   * Utility method to consume reader and unescape entities found within.
   * @param inAttribute inAttribute
   * @return unescaped string from reader
   */
  private[parser] def unescapeEntities(inAttribute: Boolean): String = {
    val builder: StringBuilder = new StringBuilder
    while (!reader.isEmpty) {
      builder.append(reader.consumeTo('&'))
      if (reader.matches('&')) {
        reader.consume
        val c: Array[Char] = consumeCharacterReference(None, inAttribute)
        if (c == null || c.length == 0) builder.append('&')
        else builder.append(c)
      }
    }
    builder.toString()
  }
}

private[parser] object Tokeniser {
  private[parser] val replacementChar: Char = '\uFFFD'
  private val notCharRefCharsSorted: Array[Char] = Array[Char]('\t', '\n', '\r', '\f', ' ', '<', '&').sorted
}
