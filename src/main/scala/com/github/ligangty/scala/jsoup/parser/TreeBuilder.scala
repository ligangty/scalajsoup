package com.github.ligangty.scala.jsoup.parser

import com.github.ligangty.scala.jsoup.helper.Validator._
import com.github.ligangty.scala.jsoup.nodes.{Attributes, Element, Document}

import scala.collection.mutable


private[parser] abstract class TreeBuilder {
  private[parser] var reader: CharacterReader = null
  private[parser] var tokeniser: Tokeniser = null
  // the last start tag emitted, to test appropriate end tag
  protected var doc: Document = null
  // the stack of open elements
  protected var stack: mutable.Buffer[Element] = null
  // current base uri, for creating new elements
  protected var baseUri: String = null
  // currentToken is used only for error tracking.
  protected var currentToken: Token = null
  // null when not tracking errors
  protected var errors: ParseErrorList = null
  // start tag to process
  private val start: Token.StartTag = new Token.StartTag
  private val end: Token.EndTag = new Token.EndTag

  protected def initialiseParse(input: String, baseUri: String, errors: ParseErrorList) {
    notNull(input, "String input must not be null")
    notNull(baseUri, "BaseURI must not be null")
    doc = new Document(baseUri)
    reader = new CharacterReader(input)
    this.errors = errors
    tokeniser = new Tokeniser(reader, errors)
    stack = new mutable.ArrayBuffer[Element](32)
    this.baseUri = baseUri
  }

  private[parser] def parse(input: String, baseUri: String): Document = parse(input, baseUri, ParseErrorList.noTracking)

  private[parser] def parse(input: String, baseUri: String, errors: ParseErrorList): Document = {
    initialiseParse(input, baseUri, errors)
    runParser()
    doc
  }

  protected def runParser(): Unit = {
    import scala.util.control.Breaks._
    breakable {
      while (true) {
        val token: Token = tokeniser.read
        process(token)
        token.reset
        if (token.tokType == Token.TokenType.EOF) break()
      }
    }
  }

  protected[parser] def process(token: Token): Boolean

  protected[parser] def processStartTag(name: String): Boolean = process(start.reset.name(name))


  def processStartTag(name: String, attrs: Attributes): Boolean = {
    start.reset
    start.nameAttr(name, attrs)
    process(start)
  }

  protected[parser] def processEndTag(name: String): Boolean = process(end.reset.name(name))

  protected[parser] def currentElement: Element = {
    val size: Int = stack.size
    if (size > 0) stack(size - 1) else null
  }
}
