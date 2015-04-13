package com.github.ligangty.scala.jsoup.parser


private[parser] object TokeniserState {

  private[parser] sealed trait State {
    protected val SHOULD_NOT_IN_ATTR = false
    protected val SHOULD_NOT_START = false

    protected val SHOULD_IN_ATTR = true
    protected val SHOULD_START = true

    private[parser] def read(t: Tokeniser, r: CharacterReader): Unit
  }

  // in data state, gather characters until a character reference or tag is found
  private[parser] case object Data extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      r.current match {
        case '&' =>
          t.advanceTransition(CharacterReferenceInData)
        case '<' =>
          t.advanceTransition(TagOpen)
        case TokeniserState.nullChar => // NOT replacement character (oddly?)
          t.error(this)
          t.emit(r.consume)
        case TokeniserState.eof =>
          t.emit(new Token.EOF)
        case _ =>
          val data: String = r.consumeData
          t.emit(data)
      }
    }
  }

  // from & in data
  private[parser] case object CharacterReferenceInData extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      val c: Array[Char] = t.consumeCharacterReference(None, SHOULD_NOT_IN_ATTR)
      if (c == null) t.emit('&') else t.emit(c)
      t.transition(Data)
    }
  }


  /// handles data in title, textarea etc
  private[parser] case object Rcdata extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      r.current match {
        case '&' =>
          t.advanceTransition(CharacterReferenceInRcdata)
        case '<' =>
          t.advanceTransition(RcdataLessthanSign)
        case TokeniserState.nullChar =>
          t.error(this)
          r.advance()
          t.emit(replacementChar)
        case TokeniserState.eof =>
          t.emit(new Token.EOF)
        case _ =>
          val data: String = r.consumeToAny('&', '<', nullChar)
          t.emit(data)
      }
    }
  }

  private[parser] case object CharacterReferenceInRcdata extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      val c: Array[Char] = t.consumeCharacterReference(null, SHOULD_NOT_IN_ATTR)
      if (c == null) t.emit('&') else t.emit(c)
      t.transition(Rcdata)
    }
  }

  private[parser] case object Rawtext extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      r.current match {
        case '<' =>
          t.advanceTransition(RawtextLessthanSign)
        case TokeniserState.nullChar =>
          t.error(this)
          r.advance()
          t.emit(replacementChar)
        case TokeniserState.eof =>
          t.emit(new Token.EOF)
        case _ =>
          val data: String = r.consumeToAny('<', nullChar)
          t.emit(data)
      }
    }
  }

  private[parser] case object ScriptData extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      r.current match {
        case '<' =>
          t.advanceTransition(ScriptDataLessthanSign)
        case TokeniserState.nullChar =>
          t.error(this)
          r.advance()
          t.emit(replacementChar)
        case TokeniserState.eof =>
          t.emit(new Token.EOF)
        case _ =>
          val data: String = r.consumeToAny('<', nullChar)
          t.emit(data)
      }
    }
  }

  private[parser] case object PLAINTEXT extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      r.current match {
        case TokeniserState.nullChar =>
          t.error(this)
          r.advance()
          t.emit(replacementChar)
        case TokeniserState.eof =>
          t.emit(new Token.EOF)
        case _ =>
          val data: String = r.consumeTo(nullChar)
          t.emit(data)
      }
    }
  }

  // from < in data
  private[parser] case object TagOpen extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      r.current match {
        case '!' =>
          t.advanceTransition(MarkupDeclarationOpen)
        case '/' =>
          t.advanceTransition(EndTagOpen)
        case '?' =>
          t.advanceTransition(BogusComment)
        case _ =>
          if (r.matchesLetter) {
            t.createTagPending(SHOULD_START)
            t.transition(TagName)
          } else {
            t.error(this)
            t.emit('<')
            t.transition(Data)
          }
      }
    }
  }

  private[parser] case object EndTagOpen extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      if (r.isEmpty) {
        t.eofError(this)
        t.emit("</")
        t.transition(Data)
      } else if (r.matchesLetter) {
        t.createTagPending(SHOULD_NOT_START)
        t.transition(TagName)
      } else if (r.matches('>')) {
        t.error(this)
        t.advanceTransition(Data)
      } else {
        t.error(this)
        t.advanceTransition(BogusComment)
      }
    }
  }

  // from < or </ in data, will have start or end tag pending
  private[parser] case object TagName extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      // previous TagOpen state did NOT consume, will have a letter char in current
      //String tagName = r.consumeToAnySorted(tagCharsSorted).toLowerCase();
      val tagName: String = r.consumeTagName.toLowerCase
      t.tagPending.appendTagName(tagName)

      r.consume match {
        case '\t' | '\n' | '\r' | '\f' | ' ' =>
          t.transition(BeforeAttributeName)
        case '/' =>
          t.transition(SelfClosingStartTag)
        case '>' =>
          t.emitTagPending()
          t.transition(Data)
        case TokeniserState.nullChar =>
          // replacement
          t.tagPending.appendTagName(replacementStr)
        case TokeniserState.eof =>
          // should emit pending tag?
          t.eofError(this)
          t.transition(Data)
      }
    }
  }

  // from < in rcdata
  private[parser] case object RcdataLessthanSign extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      if (r.matches('/')) {
        t.createTempBuffer()
        t.advanceTransition(RCDATAEndTagOpen)
      } else if (r.matchesLetter && t.appropriateEndTagName != null && !r.containsIgnoreCase("</" + t.appropriateEndTagName)) {
        // diverge from spec: got a start tag, but there's no appropriate end tag (</title>), so rather than
        // consuming to EOF; break out here
        t.tagPending = t.createTagPending(SHOULD_NOT_START).name(t.appropriateEndTagName)
        t.emitTagPending()
        r.unconsume()
        t.transition(Data)
      } else {
        t.emit("<")
        t.transition(Rcdata)
      }
    }
  }

  private[parser] case object RCDATAEndTagOpen extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      if (r.matchesLetter) {
        t.createTagPending(SHOULD_NOT_START)
        t.tagPending.appendTagName(Character.toLowerCase(r.current))
        t.dataBuffer.append(Character.toLowerCase(r.current))
        t.advanceTransition(RCDATAEndTagName)
      } else {
        t.emit("</")
        t.transition(Rcdata)
      }
    }
  }

  private[parser] case object RCDATAEndTagName extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      if (r.matchesLetter) {
        val name: String = r.consumeLetterSequence
        t.tagPending.appendTagName(name.toLowerCase)
        t.dataBuffer.append(name)
        return
      }

      val c: Char = r.consume
      c match {
        case '\t' | '\n' | '\r' | '\f' | ' ' =>
          if (t.isAppropriateEndTagToken) t.transition(BeforeAttributeName)
          else anythingElse(t, r)
        case '/' =>
          if (t.isAppropriateEndTagToken) t.transition(SelfClosingStartTag)
          else anythingElse(t, r)
        case '>' =>
          if (t.isAppropriateEndTagToken) {
            t.emitTagPending()
            t.transition(Data)
          }
          else anythingElse(t, r)
        case _ =>
          anythingElse(t, r)
      }
    }

    private def anythingElse(t: Tokeniser, r: CharacterReader) {
      t.emit("</" + t.dataBuffer.toString)
      r.unconsume()
      t.transition(Rcdata)
    }
  }

  private[parser] case object RawtextLessthanSign extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      if (r.matches('/')) {
        t.createTempBuffer()
        t.advanceTransition(RawtextEndTagOpen)
      } else {
        t.emit('<')
        t.transition(Rawtext)
      }
    }
  }

  private[parser] case object RawtextEndTagOpen extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      if (r.matchesLetter) {
        t.createTagPending(SHOULD_NOT_START)
        t.transition(RawtextEndTagName)
      } else {
        t.emit("</")
        t.transition(Rawtext)
      }
    }
  }

  private[parser] case object RawtextEndTagName extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      handleDataEndTag(t, r, Rawtext)
    }
  }

  private[parser] case object ScriptDataLessthanSign extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      r.consume match {
        case '/' =>
          t.createTempBuffer()
          t.transition(ScriptDataEndTagOpen)
        case '!' =>
          t.emit("<!")
          t.transition(ScriptDataEscapeStart)
        case _ =>
          t.emit("<")
          r.unconsume()
          t.transition(ScriptData)
      }
    }
  }

  private[parser] case object ScriptDataEndTagOpen extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      if (r.matchesLetter) {
        t.createTagPending(SHOULD_NOT_START)
        t.transition(ScriptDataEndTagName)
      } else {
        t.emit("</")
        t.transition(ScriptData)
      }
    }
  }

  private[parser] case object ScriptDataEndTagName extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      handleDataEndTag(t, r, ScriptData)
    }
  }

  private[parser] case object ScriptDataEscapeStart extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      if (r.matches('-')) {
        t.emit('-')
        t.advanceTransition(ScriptDataEscapeStartDash)
      } else {
        t.transition(ScriptData)
      }
    }
  }

  private[parser] case object ScriptDataEscapeStartDash extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      if (r.matches('-')) {
        t.emit('-')
        t.advanceTransition(ScriptDataEscapedDashDash)
      } else {
        t.transition(ScriptData)
      }
    }
  }

  private[parser] case object ScriptDataEscaped extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      if (r.isEmpty) {
        t.eofError(this)
        t.transition(Data)
        return
      }

      r.current match {
        case '-' =>
          t.emit('-')
          t.advanceTransition(ScriptDataEscapedDash)
        case '<' =>
          t.advanceTransition(ScriptDataEscapedLessthanSign)
        case TokeniserState.nullChar =>
          t.error(this)
          r.advance()
          t.emit(replacementChar)
        case _ =>
          val data: String = r.consumeToAny('-', '<', nullChar)
          t.emit(data)
      }
    }
  }

  private[parser] case object ScriptDataEscapedDash extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      if (r.isEmpty) {
        t.eofError(this)
        t.transition(Data)
        return
      }

      val c: Char = r.consume
      c match {
        case '-' =>
          t.emit(c)
          t.transition(ScriptDataEscapedDashDash)
        case '<' =>
          t.transition(ScriptDataEscapedLessthanSign)
        case TokeniserState.nullChar =>
          t.error(this)
          t.emit(replacementChar)
          t.transition(ScriptDataEscaped)
        case _ =>
          t.emit(c)
          t.transition(ScriptDataEscaped)
      }
    }
  }

  private[parser] case object ScriptDataEscapedDashDash extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      if (r.isEmpty) {
        t.eofError(this)
        t.transition(Data)
        return
      }

      val c: Char = r.consume
      c match {
        case '-' =>
          t.emit(c)
        case '<' =>
          t.transition(ScriptDataEscapedLessthanSign)
        case '>' =>
          t.emit(c)
          t.transition(ScriptData)
        case TokeniserState.nullChar =>
          t.error(this)
          t.emit(replacementChar)
          t.transition(ScriptDataEscaped)
        case _ =>
          t.emit(c)
          t.transition(ScriptDataEscaped)
      }
    }
  }

  private[parser] case object ScriptDataEscapedLessthanSign extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      if (r.matchesLetter) {
        t.createTempBuffer()
        t.dataBuffer.append(Character.toLowerCase(r.current))
        t.emit("<" + r.current)
        t.advanceTransition(ScriptDataDoubleEscapeStart)
      } else if (r.matches('/')) {
        t.createTempBuffer()
        t.advanceTransition(ScriptDataEscapedEndTagOpen)
      } else {
        t.emit('<')
        t.transition(ScriptDataEscaped)
      }
    }
  }

  private[parser] case object ScriptDataEscapedEndTagOpen extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      if (r.matchesLetter) {
        t.createTagPending(SHOULD_NOT_START)
        t.tagPending.appendTagName(Character.toLowerCase(r.current))
        t.dataBuffer.append(r.current)
        t.advanceTransition(ScriptDataEscapedEndTagName)
      } else {
        t.emit("</")
        t.transition(ScriptDataEscaped)
      }
    }
  }

  private[parser] case object ScriptDataEscapedEndTagName extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      handleDataEndTag(t, r, ScriptDataEscaped)
    }
  }

  private[parser] case object ScriptDataDoubleEscapeStart extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      handleDataDoubleEscapeTag(t, r, ScriptDataDoubleEscaped, ScriptDataEscaped)
    }
  }

  private[parser] case object ScriptDataDoubleEscaped extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      val c: Char = r.current
      c match {
        case '-' =>
          t.emit(c)
          t.advanceTransition(ScriptDataDoubleEscapedDash)
        case '<' =>
          t.emit(c)
          t.advanceTransition(ScriptDataDoubleEscapedLessthanSign)
        case TokeniserState.nullChar =>
          t.error(this)
          r.advance()
          t.emit(replacementChar)
        case TokeniserState.eof =>
          t.eofError(this)
          t.transition(Data)
        case _ =>
          val data: String = r.consumeToAny('-', '<', nullChar)
          t.emit(data)
      }
    }
  }

  private[parser] case object ScriptDataDoubleEscapedDash extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      val c: Char = r.consume
      c match {
        case '-' =>
          t.emit(c)
          t.transition(ScriptDataDoubleEscapedDashDash)
        case '<' =>
          t.emit(c)
          t.transition(ScriptDataDoubleEscapedLessthanSign)
        case TokeniserState.nullChar =>
          t.error(this)
          t.emit(replacementChar)
          t.transition(ScriptDataDoubleEscaped)
        case TokeniserState.eof =>
          t.eofError(this)
          t.transition(Data)
        case _ =>
          t.emit(c)
          t.transition(ScriptDataDoubleEscaped)
      }
    }
  }

  private[parser] case object ScriptDataDoubleEscapedDashDash extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      val c: Char = r.consume
      c match {
        case '-' =>
          t.emit(c)
        case '<' =>
          t.emit(c)
          t.transition(ScriptDataDoubleEscapedLessthanSign)
        case '>' =>
          t.emit(c)
          t.transition(ScriptData)
        case TokeniserState.nullChar =>
          t.error(this)
          t.emit(replacementChar)
          t.transition(ScriptDataDoubleEscaped)
        case TokeniserState.eof =>
          t.eofError(this)
          t.transition(Data)
        case _ =>
          t.emit(c)
          t.transition(ScriptDataDoubleEscaped)
      }
    }
  }

  private[parser] case object ScriptDataDoubleEscapedLessthanSign extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      if (r.matches('/')) {
        t.emit('/')
        t.createTempBuffer()
        t.advanceTransition(ScriptDataDoubleEscapeEnd)
      } else {
        t.transition(ScriptDataDoubleEscaped)
      }
    }
  }

  private[parser] case object ScriptDataDoubleEscapeEnd extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      handleDataDoubleEscapeTag(t, r, ScriptDataEscaped, ScriptDataDoubleEscaped)
    }
  }


  private[parser] case object BeforeAttributeName extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      val c: Char = r.consume
      c match {
        // ignore whitespace
        case '\t' | '\n' | '\r' | '\f' | ' ' => ()
        case '/' =>
          t.transition(SelfClosingStartTag)
        case '>' =>
          t.emitTagPending()
          t.transition(Data)
        case TokeniserState.nullChar =>
          t.error(this)
          t.tagPending.newAttribute()
          r.unconsume()
          t.transition(AttributeName)
        case TokeniserState.eof =>
          t.eofError(this)
          t.transition(Data)
        case '"' | '\'' | '<' | '=' =>
          t.error(this)
          t.tagPending.newAttribute()
          t.tagPending.appendAttributeName(c)
          t.transition(AttributeName)
        case _ =>
          // A-Z, anything else
          t.tagPending.newAttribute()
          r.unconsume()
          t.transition(AttributeName)
      }
    }
  }

  // from before attribute name
  private[parser] case object AttributeName extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      val name: String = r.consumeToAnySorted(attributeNameCharsSorted)
      t.tagPending.appendAttributeName(name.toLowerCase)

      val c: Char = r.consume
      c match {
        case '\t' | '\n' | '\r' | '\f' | ' ' =>
          t.transition(AfterAttributeName)
        case '/' =>
          t.transition(SelfClosingStartTag)
        case '=' =>
          t.transition(BeforeAttributeValue)
        case '>' =>
          t.emitTagPending()
          t.transition(Data)
        case TokeniserState.nullChar =>
          t.error(this)
          t.tagPending.appendAttributeName(replacementChar)
        case TokeniserState.eof =>
          t.eofError(this)
          t.transition(Data)
        case '"' | '\'' | '<' =>
          t.error(this)
          t.tagPending.appendAttributeName(c)
        // no default, as covered in consumeToAny
      }
    }
  }

  private[parser] case object AfterAttributeName extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      val c: Char = r.consume
      c match {
        // ignore
        case '\t' | '\n' | '\r' | '\f' | ' ' => ()
        case '/' =>
          t.transition(SelfClosingStartTag)
        case '=' =>
          t.transition(BeforeAttributeValue)
        case '>' =>
          t.emitTagPending()
          t.transition(Data)
        case TokeniserState.nullChar =>
          t.error(this)
          t.tagPending.appendAttributeName(replacementChar)
          t.transition(AttributeName)
        case TokeniserState.eof =>
          t.eofError(this)
          t.transition(Data)
        case '"' =>
        case '\'' =>
        case '<' =>
          t.error(this)
          t.tagPending.newAttribute()
          t.tagPending.appendAttributeName(c)
          t.transition(AttributeName)
        case _ =>
          // A-Z, anything else
          t.tagPending.newAttribute()
          r.unconsume()
          t.transition(AttributeName)
      }
    }
  }

  private[parser] case object BeforeAttributeValue extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      val c: Char = r.consume
      c match {
        // ignore
        case '\t' | '\n' | '\r' | '\f' | ' ' => ()
        case '"' =>
          t.transition(AttributeValue_doubleQuoted)
        case '&' =>
          r.unconsume()
          t.transition(AttributeValue_unquoted)
        case '\'' =>
          t.transition(AttributeValue_singleQuoted)
        case TokeniserState.nullChar =>
          t.error(this)
          t.tagPending.appendAttributeValue(replacementChar)
          t.transition(AttributeValue_unquoted)
        case TokeniserState.eof =>
          t.eofError(this)
          t.transition(Data)
        case '>' =>
          t.error(this)
          t.emitTagPending()
          t.transition(Data)
        case '<' =>
        case '=' =>
        case '`' =>
          t.error(this)
          t.tagPending.appendAttributeValue(c)
          t.transition(AttributeValue_unquoted)
        case _ =>
          r.unconsume()
          t.transition(AttributeValue_unquoted)
      }
    }
  }

  private[parser] case object AttributeValue_doubleQuoted extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      val value: String = r.consumeToAnySorted(attributeDoubleValueCharsSorted)
      if (value.length > 0) t.tagPending.appendAttributeValue(value)

      val c: Char = r.consume
      c match {
        case '"' =>
          t.transition(AfterAttributeValue_quoted)
        case '&' =>
          val ref: Array[Char] = t.consumeCharacterReference(Some('"'), SHOULD_IN_ATTR)
          if (ref != null) t.tagPending.appendAttributeValue(ref)
          else t.tagPending.appendAttributeValue('&')
        case TokeniserState.nullChar =>
          t.error(this)
          t.tagPending.appendAttributeValue(replacementChar)
        case TokeniserState.eof =>
          t.eofError(this)
          t.transition(Data)
        // no default, handled in consume to any above
      }
    }
  }

  private[parser] case object AttributeValue_singleQuoted extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      val value: String = r.consumeToAnySorted(attributeSingleValueCharsSorted)
      if (value.length > 0) t.tagPending.appendAttributeValue(value)

      val c: Char = r.consume
      c match {
        case '\'' =>
          t.transition(AfterAttributeValue_quoted)
        case '&' =>
          val ref: Array[Char] = t.consumeCharacterReference(Some('\''), SHOULD_IN_ATTR)
          if (ref != null) t.tagPending.appendAttributeValue(ref)
          else t.tagPending.appendAttributeValue('&')
        case TokeniserState.nullChar =>
          t.error(this)
          t.tagPending.appendAttributeValue(replacementChar)
        case TokeniserState.eof =>
          t.eofError(this)
          t.transition(Data)
        // no default, handled in consume to any above
      }
    }
  }

  private[parser] case object AttributeValue_unquoted extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      val value: String = r.consumeToAny('\t', '\n', '\r', '\f', ' ', '&', '>', nullChar, '"', '\'', '<', '=', '`')
      if (value.length > 0) t.tagPending.appendAttributeValue(value)

      val c: Char = r.consume
      c match {
        case '\t' | '\n' | '\r' | '\f' | ' ' => ()
          t.transition(BeforeAttributeName)
        case '&' =>
          val ref: Array[Char] = t.consumeCharacterReference(Some('>'), SHOULD_IN_ATTR)
          if (ref != null) t.tagPending.appendAttributeValue(ref)
          else t.tagPending.appendAttributeValue('&')
        case '>' =>
          t.emitTagPending()
          t.transition(Data)
        case TokeniserState.nullChar =>
          t.error(this)
          t.tagPending.appendAttributeValue(replacementChar)
        case TokeniserState.eof =>
          t.eofError(this)
          t.transition(Data)
        case '"' | '\'' | '<' | '=' | '`' =>
          t.error(this)
          t.tagPending.appendAttributeValue(c)
        // no default, handled in consume to any above
      }
    }
  }

  // CharacterReferenceInAttributeValue state handled inline
  private[parser] case object AfterAttributeValue_quoted extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      val c: Char = r.consume
      c match {
        case '\t' | '\n' | '\r' | '\f' | ' ' =>
          t.transition(BeforeAttributeName)
        case '/' =>
          t.transition(SelfClosingStartTag)
        case '>' =>
          t.emitTagPending()
          t.transition(Data)
        case TokeniserState.eof =>
          t.eofError(this)
          t.transition(Data)
        case _ =>
          t.error(this)
          r.unconsume()
          t.transition(BeforeAttributeName)
      }
    }
  }

  private[parser] case object SelfClosingStartTag extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      val c: Char = r.consume
      c match {
        case '>' =>
          t.tagPending.selfClosing = true
          t.emitTagPending()
          t.transition(Data)
        case TokeniserState.eof =>
          t.eofError(this)
          t.transition(Data)
        case _ =>
          t.error(this)
          t.transition(BeforeAttributeName)
      }
    }
  }

  private[parser] case object BogusComment extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      // todo: handle bogus comment starting from eof. when does that trigger?
      // rewind to capture character that lead us here
      r.unconsume()
      val comment: Token.Comment = new Token.Comment
      comment.bogus = true
      comment.data.append(r.consumeTo('>'))
      // todo: replace nullChar with replaceChar
      t.emit(comment)
      t.advanceTransition(Data)
    }
  }

  private[parser] case object MarkupDeclarationOpen extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      if (r.matchConsume("--")) {
        t.createCommentPending()
        t.transition(CommentStart)
      } else if (r.matchConsumeIgnoreCase("DOCTYPE")) {
        t.transition(Doctype)
      } else if (r.matchConsume("[CDATA[")) {
        // todo: should actually check current namepspace, and only non-html allows cdata. until namespace
        // is implemented properly, keep handling as cdata
        //} else if (!t.currentNodeInHtmlNS() && r.matchConsume("[CDATA[")) {
        t.transition(CdataSection)
      } else {
        t.error(this)
        // advance so this character gets in bogus comment data's rewind
        t.advanceTransition(BogusComment)
      }
    }
  }

  private[parser] case object CommentStart extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      val c: Char = r.consume
      c match {
        case '-' =>
          t.transition(CommentStartDash)
        case TokeniserState.nullChar =>
          t.error(this)
          t.commentPending.data.append(replacementChar)
          t.transition(Comment)
        case '>' =>
          t.error(this)
          t.emitCommentPending()
          t.transition(Data)
        case TokeniserState.eof =>
          t.eofError(this)
          t.emitCommentPending()
          t.transition(Data)
        case _ =>
          t.commentPending.data.append(c)
          t.transition(Comment)
      }
    }
  }

  private[parser] case object CommentStartDash extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      val c: Char = r.consume
      c match {
        case '-' =>
          t.transition(CommentStartDash)
        case TokeniserState.nullChar =>
          t.error(this)
          t.commentPending.data.append(replacementChar)
          t.transition(Comment)
        case '>' =>
          t.error(this)
          t.emitCommentPending()
          t.transition(Data)
        case TokeniserState.eof =>
          t.eofError(this)
          t.emitCommentPending()
          t.transition(Data)
        case _ =>
          t.commentPending.data.append(c)
          t.transition(Comment)
      }
    }
  }

  private[parser] case object Comment extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      val c: Char = r.current
      c match {
        case '-' =>
          t.advanceTransition(CommentEndDash)
        case TokeniserState.nullChar =>
          t.error(this)
          r.advance()
          t.commentPending.data.append(replacementChar)
        case TokeniserState.eof =>
          t.eofError(this)
          t.emitCommentPending()
          t.transition(Data)
        case _ =>
          t.commentPending.data.append(r.consumeToAny('-', nullChar))
      }
    }
  }

  private[parser] case object CommentEndDash extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      val c: Char = r.consume
      c match {
        case '-' =>
          t.transition(CommentEnd)
        case TokeniserState.nullChar =>
          t.error(this)
          t.commentPending.data.append('-').append(replacementChar)
          t.transition(Comment)
        case TokeniserState.eof =>
          t.eofError(this)
          t.emitCommentPending()
          t.transition(Data)
        case _ =>
          t.commentPending.data.append('-').append(c)
          t.transition(Comment)
      }
    }
  }

  private[parser] case object CommentEnd extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      val c: Char = r.consume
      c match {
        case '>' =>
          t.emitCommentPending()
          t.transition(Data)
        case TokeniserState.nullChar =>
          t.error(this)
          t.commentPending.data.append("--").append(replacementChar)
          t.transition(Comment)
        case '!' =>
          t.error(this)
          t.transition(CommentEndBang)
        case '-' =>
          t.error(this)
          t.commentPending.data.append('-')
        case TokeniserState.eof =>
          t.eofError(this)
          t.emitCommentPending()
          t.transition(Data)
        case _ =>
          t.error(this)
          t.commentPending.data.append("--").append(c)
          t.transition(Comment)
      }
    }
  }

  private[parser] case object CommentEndBang extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      val c: Char = r.consume
      c match {
        case '-' =>
          t.commentPending.data.append("--!")
          t.transition(CommentEndDash)
        case '>' =>
          t.emitCommentPending()
          t.transition(Data)
        case TokeniserState.nullChar =>
          t.error(this)
          t.commentPending.data.append("--!").append(replacementChar)
          t.transition(Comment)
        case TokeniserState.eof =>
          t.eofError(this)
          t.emitCommentPending()
          t.transition(Data)
        case _ =>
          t.commentPending.data.append("--!").append(c)
          t.transition(Comment)
      }
    }
  }

  private[parser] case object Doctype extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      val c: Char = r.consume
      c match {
        case '\t' | '\n' | '\r' | '\f' | ' ' =>
          t.transition(BeforeDoctypeName)
        case TokeniserState.eof =>
          t.eofError(this)
        case '>' =>
          t.error(this)
          t.createDoctypePending()
          t.doctypePending.forceQuirks = true
          t.emitDoctypePending()
          t.transition(Data)
        case _ =>
          t.error(this)
          t.transition(BeforeDoctypeName)
      }
    }
  }

  private[parser] case object BeforeDoctypeName extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      if (r.matchesLetter) {
        t.createDoctypePending()
        t.transition(DoctypeName)
        return
      }
      val c: Char = r.consume
      c match {
        case '\t' | '\n' | '\r' | '\f' | ' ' => ()
        case TokeniserState.nullChar =>
          t.error(this)
          t.createDoctypePending()
          t.doctypePending.name.append(replacementChar)
          t.transition(DoctypeName)
        case TokeniserState.eof =>
          t.eofError(this)
          t.createDoctypePending()
          t.doctypePending.forceQuirks = true
          t.emitDoctypePending()
          t.transition(Data)
        case _ =>
          t.createDoctypePending()
          t.doctypePending.name.append(c)
          t.transition(DoctypeName)
      }
    }
  }

  private[parser] case object DoctypeName extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      if (r.matchesLetter) {
        val name: String = r.consumeLetterSequence
        t.doctypePending.name.append(name.toLowerCase)
        return
      }
      val c: Char = r.consume
      c match {
        case '>' =>
          t.emitDoctypePending()
          t.transition(Data)
        case '\t' | '\n' | '\r' | '\f' | ' ' =>
          t.transition(AfterDoctypeName)
        case TokeniserState.nullChar =>
          t.error(this)
          t.doctypePending.name.append(replacementChar)
        case TokeniserState.eof =>
          t.eofError(this)
          t.doctypePending.forceQuirks = true
          t.emitDoctypePending()
          t.transition(Data)
        case _ =>
          t.doctypePending.name.append(c)
      }
    }
  }

  private[parser] case object AfterDoctypeName extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      if (r.isEmpty) {
        t.eofError(this)
        t.doctypePending.forceQuirks = true
        t.emitDoctypePending()
        t.transition(Data)
        return
      }
      if (r.matchesAny('\t', '\n', '\r', '\f', ' ')) {
        r.advance()
      } else if (r.matches('>')) {
        t.emitDoctypePending()
        t.advanceTransition(Data)
      } else if (r.matchConsumeIgnoreCase("PUBLIC")) {
        t.transition(AfterDoctypePublicKeyword)
      } else if (r.matchConsumeIgnoreCase("SYSTEM")) {
        t.transition(AfterDoctypeSystemKeyword)
      } else {
        t.error(this)
        t.doctypePending.forceQuirks = true
        t.advanceTransition(BogusDoctype)
      }
    }
  }

  private[parser] case object AfterDoctypePublicKeyword extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      val c: Char = r.consume
      c match {
        case '\t' | '\n' | '\r' | '\f' | ' ' =>
          t.transition(BeforeDoctypePublicIdentifier)
        case '"' =>
          t.error(this)
          t.transition(DoctypePublicIdentifier_doubleQuoted)
        case '\'' =>
          t.error(this)
          t.transition(DoctypePublicIdentifier_singleQuoted)
        case '>' =>
          t.error(this)
          t.doctypePending.forceQuirks = true
          t.emitDoctypePending()
          t.transition(Data)
        case TokeniserState.eof =>
          t.eofError(this)
          t.doctypePending.forceQuirks = true
          t.emitDoctypePending()
          t.transition(Data)
        case _ =>
          t.error(this)
          t.doctypePending.forceQuirks = true
          t.transition(BogusDoctype)
      }
    }
  }

  private[parser] case object BeforeDoctypePublicIdentifier extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      val c: Char = r.consume
      c match {
        case '\t' | '\n' | '\r' | '\f' | ' ' => ()
        case '"' =>
          // set public id to empty string
          t.transition(DoctypePublicIdentifier_doubleQuoted)
        case '\'' =>
          // set public id to empty string
          t.transition(DoctypePublicIdentifier_singleQuoted)
        case '>' =>
          t.error(this)
          t.doctypePending.forceQuirks = true
          t.emitDoctypePending()
          t.transition(Data)
        case TokeniserState.eof =>
          t.eofError(this)
          t.doctypePending.forceQuirks = true
          t.emitDoctypePending()
          t.transition(Data)
        case _ =>
          t.error(this)
          t.doctypePending.forceQuirks = true
          t.transition(BogusDoctype)
      }
    }
  }

  private[parser] case object DoctypePublicIdentifier_doubleQuoted extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      val c: Char = r.consume
      c match {
        case '"' =>
          t.transition(AfterDoctypePublicIdentifier)
        case TokeniserState.nullChar =>
          t.error(this)
          t.doctypePending.publicIdentifier.append(replacementChar)
        case '>' =>
          t.error(this)
          t.doctypePending.forceQuirks = true
          t.emitDoctypePending()
          t.transition(Data)
        case TokeniserState.eof =>
          t.eofError(this)
          t.doctypePending.forceQuirks = true
          t.emitDoctypePending()
          t.transition(Data)
        case _ =>
          t.doctypePending.publicIdentifier.append(c)
      }
    }
  }

  private[parser] case object DoctypePublicIdentifier_singleQuoted extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      val c: Char = r.consume
      c match {
        case '\'' =>
          t.transition(AfterDoctypePublicIdentifier)
        case TokeniserState.nullChar =>
          t.error(this)
          t.doctypePending.publicIdentifier.append(replacementChar)
        case '>' =>
          t.error(this)
          t.doctypePending.forceQuirks = true
          t.emitDoctypePending()
          t.transition(Data)
        case TokeniserState.eof =>
          t.eofError(this)
          t.doctypePending.forceQuirks = true
          t.emitDoctypePending()
          t.transition(Data)
        case _ =>
          t.doctypePending.publicIdentifier.append(c)
      }
    }
  }

  private[parser] case object AfterDoctypePublicIdentifier extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      val c: Char = r.consume
      c match {
        case '\t' | '\n' | '\r' | '\f' | ' ' =>
          t.transition(BetweenDoctypePublicAndSystemIdentifiers)
        case '>' =>
          t.emitDoctypePending()
          t.transition(Data)
        case '"' =>
          t.error(this)
          t.transition(DoctypeSystemIdentifier_doubleQuoted)
        case '\'' =>
          t.error(this)
          t.transition(DoctypeSystemIdentifier_singleQuoted)
        case TokeniserState.eof =>
          t.eofError(this)
          t.doctypePending.forceQuirks = true
          t.emitDoctypePending()
          t.transition(Data)
        case _ =>
          t.error(this)
          t.doctypePending.forceQuirks = true
          t.transition(BogusDoctype)
      }
    }
  }

  private[parser] case object BetweenDoctypePublicAndSystemIdentifiers extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      val c: Char = r.consume
      c match {
        case '\t' | '\n' | '\r' | '\f' | ' ' => ()
        case '>' =>
          t.emitDoctypePending()
          t.transition(Data)
        case '"' =>
          t.error(this)
          t.transition(DoctypeSystemIdentifier_doubleQuoted)
        case '\'' =>
          t.error(this)
          t.transition(DoctypeSystemIdentifier_singleQuoted)
        case TokeniserState.eof =>
          t.eofError(this)
          t.doctypePending.forceQuirks = true
          t.emitDoctypePending()
          t.transition(Data)
        case _ =>
          t.error(this)
          t.doctypePending.forceQuirks = true
          t.transition(BogusDoctype)
      }
    }
  }

  private[parser] case object AfterDoctypeSystemKeyword extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      val c: Char = r.consume
      c match {
        case '\t' | '\n' | '\r' | '\f' | ' ' =>
          t.transition(BeforeDoctypeSystemIdentifier)
        case '>' =>
          t.error(this)
          t.doctypePending.forceQuirks = true
          t.emitDoctypePending()
          t.transition(Data)
        case '"' =>
          t.error(this)
          t.transition(DoctypeSystemIdentifier_doubleQuoted)
        case '\'' =>
          t.error(this)
          t.transition(DoctypeSystemIdentifier_singleQuoted)
        case TokeniserState.eof =>
          t.eofError(this)
          t.doctypePending.forceQuirks = true
          t.emitDoctypePending()
          t.transition(Data)
        case _ =>
          t.error(this)
          t.doctypePending.forceQuirks = true
          t.emitDoctypePending()
      }
    }
  }

  private[parser] case object BeforeDoctypeSystemIdentifier extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      val c: Char = r.consume
      c match {
        case '\t' | '\n' | '\r' | '\f' | ' ' => ()
        case '"' =>
          // set system id to empty string
          t.transition(DoctypeSystemIdentifier_doubleQuoted)
        case '\'' =>
          // set public id to empty string
          t.transition(DoctypeSystemIdentifier_singleQuoted)
        case '>' =>
          t.error(this)
          t.doctypePending.forceQuirks = true
          t.emitDoctypePending()
          t.transition(Data)
        case TokeniserState.eof =>
          t.eofError(this)
          t.doctypePending.forceQuirks = true
          t.emitDoctypePending()
          t.transition(Data)
        case _ =>
          t.error(this)
          t.doctypePending.forceQuirks = true
          t.transition(BogusDoctype)
      }
    }
  }

  private[parser] case object DoctypeSystemIdentifier_doubleQuoted extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      val c: Char = r.consume
      c match {
        case '"' =>
          t.transition(AfterDoctypeSystemIdentifier)
        case TokeniserState.nullChar =>
          t.error(this)
          t.doctypePending.systemIdentifier.append(replacementChar)
        case '>' =>
          t.error(this)
          t.doctypePending.forceQuirks = true
          t.emitDoctypePending()
          t.transition(Data)
        case TokeniserState.eof =>
          t.eofError(this)
          t.doctypePending.forceQuirks = true
          t.emitDoctypePending()
          t.transition(Data)
        case _ =>
          t.doctypePending.systemIdentifier.append(c)
      }
    }
  }


  private[parser] case object DoctypeSystemIdentifier_singleQuoted extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      val c: Char = r.consume
      c match {
        case '\'' =>
          t.transition(AfterDoctypeSystemIdentifier)
        case TokeniserState.nullChar =>
          t.error(this)
          t.doctypePending.systemIdentifier.append(replacementChar)
        case '>' =>
          t.error(this)
          t.doctypePending.forceQuirks = true
          t.emitDoctypePending()
          t.transition(Data)
        case TokeniserState.eof =>
          t.eofError(this)
          t.doctypePending.forceQuirks = true
          t.emitDoctypePending()
          t.transition(Data)
        case _ =>
          t.doctypePending.systemIdentifier.append(c)
      }
    }
  }

  private[parser] case object AfterDoctypeSystemIdentifier extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      val c: Char = r.consume
      c match {
        case '\t' | '\n' | '\r' | '\f' | ' ' => ()
        case '>' =>
          t.emitDoctypePending()
          t.transition(Data)
        case TokeniserState.eof =>
          t.eofError(this)
          t.doctypePending.forceQuirks = true
          t.emitDoctypePending()
          t.transition(Data)
        case _ =>
          t.error(this)
          t.transition(BogusDoctype)
        // NOT force quirks
      }
    }
  }

  private[parser] case object BogusDoctype extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      val c: Char = r.consume
      c match {
        case '>' =>
          t.emitDoctypePending()
          t.transition(Data)
        case TokeniserState.eof =>
          t.emitDoctypePending()
          t.transition(Data)
        case _ =>
      }
    }
  }

  private[parser] case object CdataSection extends State {
    override private[parser] def read(t: Tokeniser, r: CharacterReader): Unit = {
      val data: String = r.consumeTo("]]>")
      t.emit(data)
      r.matchConsume("]]>")
      t.transition(Data)
    }
  }

  private[parser] val nullChar: Char = '\u0000'
  // todo these val should be limited access in only this class, but scala does not support, so need to open to parser
  private[TokeniserState] val attributeSingleValueCharsSorted: Array[Char] = Array[Char]('\'', '&', nullChar).sorted
  private[TokeniserState] val attributeDoubleValueCharsSorted: Array[Char] = Array[Char]('"', '&', nullChar).sorted
  private[TokeniserState] val attributeNameCharsSorted: Array[Char] = Array[Char]('\t', '\n', '\r', '\f', ' ', '/', '=', '>', nullChar, '"', '\'', '<').sorted
  private[TokeniserState] val replacementChar: Char = Tokeniser.replacementChar
  private[TokeniserState] val replacementStr: String = String.valueOf(Tokeniser.replacementChar)
  private[TokeniserState] val eof: Char = CharacterReader.EOF

  // todo these two methods should be limited access in only this class, but scala does not support, so need to open to parser
  /**
   * Handles RawtextEndTagName, ScriptDataEndTagName, and ScriptDataEscapedEndTagName. Same body impl, just
   * different else exit transitions.
   */
  private[TokeniserState] def handleDataEndTag(t: Tokeniser, r: CharacterReader, elseTransition: State) {
    if (r.matchesLetter) {
      val name: String = r.consumeLetterSequence
      t.tagPending.appendTagName(name.toLowerCase)
      t.dataBuffer.append(name)
      return
    }
    var needsExitTransition: Boolean = false
    if (t.isAppropriateEndTagToken && !r.isEmpty) {
      val c: Char = r.consume
      c match {
        case '\t' | '\n' | '\r' | '\f' | ' ' =>
          t.transition(BeforeAttributeName)
        case '/' =>
          t.transition(SelfClosingStartTag)
        case '>' =>
          t.emitTagPending()
          t.transition(Data)
        case _ =>
          t.dataBuffer.append(c)
          needsExitTransition = true
      }
    } else {
      needsExitTransition = true
    }
    if (needsExitTransition) {
      t.emit("</" + t.dataBuffer.toString)
      t.transition(elseTransition)
    }
  }

  private[TokeniserState] def handleDataDoubleEscapeTag(t: Tokeniser, r: CharacterReader, primary: State, fallback: State) {
    if (r.matchesLetter) {
      val name: String = r.consumeLetterSequence
      t.dataBuffer.append(name.toLowerCase)
      t.emit(name)
      return
    }
    val c: Char = r.consume
    c match {
      case '\t' | '\n' | '\r' | '\f' | ' ' | '/' | '>' =>
        if (t.dataBuffer.toString == "script") t.transition(primary)
        else t.transition(fallback)
        t.emit(c)
      case _ =>
        r.unconsume()
        t.transition(fallback)
    }
  }
}