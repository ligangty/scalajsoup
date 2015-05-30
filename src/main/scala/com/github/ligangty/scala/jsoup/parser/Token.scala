package com.github.ligangty.scala.jsoup.parser

import com.github.ligangty.scala.jsoup.helper.Validator._
import com.github.ligangty.scala.jsoup.nodes.{Attribute, Attributes}
import Token._

/**
 * Parse tokens for the Tokeniser.
 */
abstract private[parser] class Token private() {

  private[parser] var tokType: TokenType.Value = null

  private[parser] def tokenType: String = this.getClass.getSimpleName

  /**
   * Reset the data represent by this token, for reuse. Prevents the need to create transfer objects for every
   * piece of data, which immediately get GCed.
   */
  private[parser] def reset: Token

  private[parser] def isDoctype: Boolean = tokType == TokenType.Doctype

  private[parser] def asDoctype: Token.Doctype = this.asInstanceOf[Token.Doctype]

  private[parser] def isStartTag: Boolean = tokType == TokenType.StartTag

  private[parser] def asStartTag: Token.StartTag = this.asInstanceOf[Token.StartTag]

  private[parser] def isEndTag: Boolean = tokType == TokenType.EndTag

  private[parser] def asEndTag: Token.EndTag = this.asInstanceOf[Token.EndTag]

  private[parser] def isComment: Boolean = tokType == TokenType.Comment

  private[parser] def asComment: Token.Comment = this.asInstanceOf[Token.Comment]

  private[parser] def isCharacter: Boolean = tokType == TokenType.Character

  private[parser] def asCharacter: Token.Character = this.asInstanceOf[Token.Character]

  private[parser] def isEOF: Boolean = tokType == TokenType.EOF

}

private[parser] object Token {

  private[parser] def reset(sb: StringBuilder): Unit = if (sb != null) {
    sb.delete(0, sb.length)
  }

  private[parser] object TokenType extends Enumeration {

    val Doctype, StartTag, EndTag, Comment, Character, EOF = Value
  }

  private[parser] final class Doctype private[parser]() extends Token {

    tokType = TokenType.Doctype
    private[parser] val name: StringBuilder = new StringBuilder
    private[parser] val publicIdentifier: StringBuilder = new StringBuilder
    private[parser] val systemIdentifier: StringBuilder = new StringBuilder
    private[parser] var forceQuirks: Boolean = false

    override private[parser] def reset: Token = {
      Token.reset(name)
      Token.reset(publicIdentifier)
      Token.reset(systemIdentifier)
      forceQuirks = false
      this
    }

    private[parser] def getName: String = name.toString()

    private[parser] def getPublicIdentifier: String = publicIdentifier.toString()

    def getSystemIdentifier: String = systemIdentifier.toString()

    def isForceQuirks: Boolean = forceQuirks
  }

  private[parser] abstract class Tag extends Token {

    protected[parser] var tagName: String = null
    // attribute names are generally caught in one hop, not accumulated
    private var pendingAttributeName: String = null
    // but values are accumulated, from e.g. & in hrefs
    private var pendingAttributeValue: StringBuilder = new StringBuilder
    private var hasPendingAttributeValue: Boolean = false
    private[parser] var selfClosing: Boolean = false
    // start tags get attributes on construction. End tags get attributes on first new attribute (but only for parser convenience, not used).
    private[parser] var attributes: Attributes = null

    override private[parser] def reset: Token.Tag = {
      tagName = null
      pendingAttributeName = null
      Token.reset(pendingAttributeValue)
      hasPendingAttributeValue = false
      selfClosing = false
      attributes = null
      this
    }

    private[parser] final def newAttribute(): Unit = {
      if (attributes == null) {
        attributes = new Attributes
      }
      if (pendingAttributeName != null) {
        var attribute: Attribute = null
        if (!hasPendingAttributeValue) {
          attribute = new Attribute(pendingAttributeName, "")
        }
        else {
          attribute = new Attribute(pendingAttributeName, pendingAttributeValue.toString())
        }
        attributes.put(attribute)
      }
      pendingAttributeName = null
      Token.reset(pendingAttributeValue)
    }

    private[parser] final def finaliseTag(): Unit = {
      // finalises for emit
      if (pendingAttributeName != null) {
        //todo: check if attribute name exists; if so, drop and error
        newAttribute()
      }
    }

    private[parser] final def name: String = {
      isFalse(tagName == null || tagName.length == 0)
      tagName
    }

    private[parser] final def name(name: String): Token.Tag = {
      tagName = name
      this
    }

    private[parser] final def isSelfClosing: Boolean = selfClosing

    private[parser] final def getAttributes: Attributes = attributes

    // these appenders are rarely hit in not null state-- caused by null chars.
    private[parser] final def appendTagName(append: String): Unit = {
      tagName = if (tagName == null) {
        append
      } else {
        tagName.concat(append)
      }
    }

    private[parser] final def appendTagName(append: Char): Unit = {
      appendTagName(String.valueOf(append))
    }

    private[parser] final def appendAttributeName(append: String) {
      pendingAttributeName = if (pendingAttributeName == null) {
        append
      } else {
        pendingAttributeName.concat(append)
      }
    }

    private[parser] final def appendAttributeName(append: Char) {
      appendAttributeName(String.valueOf(append))
    }

    private[parser] final def appendAttributeValue(append: String) {
      ensureAttributeValue()
      pendingAttributeValue.append(append)
    }

    private[parser] final def appendAttributeValue(append: Char) {
      ensureAttributeValue()
      pendingAttributeValue.append(append)
    }

    private[parser] final def appendAttributeValue(append: Array[Char]) {
      ensureAttributeValue()
      // append is a array, strinbuilder cannot append it directly, so use ++=
      pendingAttributeValue ++= append
    }

    private def ensureAttributeValue(): Unit = {
      hasPendingAttributeValue = true
    }
  }

  private[parser] final class StartTag private[parser]() extends Tag {

    attributes = new Attributes
    tokType = TokenType.StartTag

    override private[parser] def reset: Token.Tag = {
      super.reset
      attributes = new Attributes
      // todo - would prefer these to be null, but need to check Element assertions
      this
    }

    private[parser] def nameAttr(name: String, attributes: Attributes): Token.StartTag = {
      this.tagName = name
      this.attributes = attributes
      this
    }

    override def toString: String = {
      if (attributes != null && attributes.size > 0) {
        "<" + name + " " + attributes.toString() + ">"
      }
      else {
        "<" + name + ">"
      }
    }
  }

  private[parser] final class EndTag private[parser]() extends Tag {

    tokType = TokenType.EndTag

    override def toString: String = "</" + name + ">"
  }

  private[parser] final class Comment private[parser]() extends Token {

    tokType = TokenType.Comment

    private[parser] val data: StringBuilder = new StringBuilder
    private[parser] var bogus: Boolean = false

    override private[parser] def reset: Token = {
      Token.reset(data)
      bogus = false
      this
    }

    private[parser] def getData: String = data.toString()

    override def toString: String = "<!--" + getData + "-->"
  }

  private[parser] final class Character private[parser]() extends Token {

    tokType = TokenType.Character

    private var data: String = null

    override private[parser] def reset: Token = {
      data = null
      this
    }

    private[parser] def data(data: String): Token.Character = {
      this.data = data
      this
    }

    private[parser] def getData: String = data

    override def toString: String = getData
  }

  private[parser] final class EOF private[parser]() extends Token {

    tokType = TokenType.EOF

    override private[parser] def reset: Token = this
  }

}
