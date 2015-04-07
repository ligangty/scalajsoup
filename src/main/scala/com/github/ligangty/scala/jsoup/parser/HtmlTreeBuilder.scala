package com.github.ligangty.scala.jsoup.parser

import com.github.ligangty.scala.jsoup.helper.{Validator, Strings}
import com.github.ligangty.scala.jsoup.nodes._
import com.github.ligangty.scala.jsoup.select.Elements

import scala.collection.mutable
import scala.util.control.Breaks._

/**
 * HTML Tree Builder; creates a DOM from Tokens.
 */
private[parser] class HtmlTreeBuilder extends TreeBuilder {

  private var stateVal: HtmlTreeBuilderState.BuilderState = null
  private var originalStateVal: HtmlTreeBuilderState.BuilderState = null
  private var baseUriSetFromDoc: Boolean = false
  private var headElement: Element = null
  private var formElement: FormElement = null
  private var contextElement: Element = null
  private var formattingElements: mutable.ArrayBuffer[Element] = new mutable.ArrayBuffer[Element]
  private var pendingTableCharacters: mutable.Buffer[String] = new mutable.ArrayBuffer[String]
  private var emptyEnd: Token.EndTag = new Token.EndTag
  private var framesetOkVal: Boolean = true
  private var fosterInserts: Boolean = false
  private var fragmentParsing: Boolean = false

  private[parser] override def parse(input: String, baseUri: String, errors: ParseErrorList): Document = {
    stateVal = HtmlTreeBuilderState.Initial
    baseUriSetFromDoc = false
    super.parse(input, baseUri, errors)
  }

  private[parser] def parseFragment(inputFragment: String, context: Element, baseUri: String, errors: ParseErrorList): Seq[Node] = {
    stateVal = HtmlTreeBuilderState.Initial
    initialiseParse(inputFragment, baseUri, errors)
    contextElement = context
    fragmentParsing = true
    var root: Element = null
    if (context != null) {
      if (context.ownerDocument != null) {
        doc.quirksMode(context.ownerDocument.quirksMode())
      }
      val contextTag: String = context.tagName
      if (Strings.in(contextTag, "title", "textarea")) {
        tokeniser.transition(TokeniserState.Rcdata)
      } else if (Strings.in(contextTag, "iframe", "noembed", "noframes", "style", "xmp")) {
        tokeniser.transition(TokeniserState.Rawtext)
      } else if (contextTag == "script") {
        tokeniser.transition(TokeniserState.ScriptData)
      } else if (contextTag == "noscript") {
        tokeniser.transition(TokeniserState.Data)
      } else if (contextTag == "plaintext") {
        tokeniser.transition(TokeniserState.Data)
      } else {
        tokeniser.transition(TokeniserState.Data)
      }
      root = new Element(Tag("html"), baseUri)
      doc.appendChild(root)
      stack.append(root)
      resetInsertionMode
      val contextChain: Elements = context.parents
      contextChain.add(0, context)
      breakable {
        for (parent <- contextChain) {
          parent match {
            case fe: FormElement =>
              formElement = fe
              break()
          }
        }
      }
    }
    runParser()
    if (context != null && root != null) {
      root.getChildNodes
    } else {
      doc.getChildNodes
    }
  }

  protected[parser] def process(token: Token): Boolean = {
    currentToken = token
    this.stateVal.process(token, this)
  }

  private[parser] def process(token: Token, state: HtmlTreeBuilderState.BuilderState): Boolean = {
    currentToken = token
    stateVal.process(token, this)
  }

  private[parser] def transition(state: HtmlTreeBuilderState.BuilderState) {
    this.stateVal = state
  }

  private[parser] def state: HtmlTreeBuilderState.BuilderState = {
    stateVal
  }

  private[parser] def markInsertionMode(): Unit = {
    originalStateVal = stateVal
  }

  private[parser] def originalState: HtmlTreeBuilderState.BuilderState = {
    return originalState
  }

  private[parser] def framesetOk(framesetOk: Boolean) {
    this.framesetOkVal = framesetOk
  }

  private[parser] def framesetOk: Boolean = {
    return framesetOkVal
  }

  private[parser] def getDocument: Document = {
    return doc
  }

  private[parser] def getBaseUri: String = {
    return baseUri
  }

  private[parser] def maybeSetBaseUri(base: Element) {
    if (baseUriSetFromDoc) {
      return
    }
    val href: String = base.absUrl("href")
    if (href.length != 0) {
      baseUri = href
      baseUriSetFromDoc = true
      doc.setBaseUri(href)
    }
  }

  private[parser] def isFragmentParsing: Boolean = {
    return fragmentParsing
  }

  private[parser] def error(state: HtmlTreeBuilderState.BuilderState) {
    if (errors.canAddError) {
      errors.append(new ParseError(reader.pos, "Unexpected token [%s] when in state [%s]", currentToken.tokenType, state))
    }
  }

  private[parser] def insert(startTag: Token.StartTag): Element = {
    // handle empty unknown tags
    // when the spec expects an empty tag, will directly hit insertEmpty, so won't generate this fake end tag.
    if (startTag.isSelfClosing) {
      val el: Element = insertEmpty(startTag)
      stack.append(el)
      // handles <script />, otherwise needs breakout steps from script data
      tokeniser.transition(TokeniserState.Data)
      // ensure we get out of whatever state we are in. emitted for yielded processing
      tokeniser.emit(emptyEnd.reset.name(el.tagName))
      return el
    }
    val el: Element = new Element(Tag(startTag.name), baseUri, startTag.attributes)
    insert(el)
    el
    null
  }

  private[parser] def insertStartTag(startTagName: String): Element = {
    val el: Element = new Element(Tag(startTagName), baseUri)
    insert(el)
    return el
  }

  private[parser] def insert(el: Element) {
    insertNode(el)
    stack.append(el)
  }

  private[parser] def insertEmpty(startTag: Token.StartTag): Element = {
    val tag: Tag = Tag(startTag.name)
    val el: Element = new Element(tag, baseUri, startTag.attributes)
    insertNode(el)
    if (startTag.isSelfClosing) {
      if (tag.isKnownTag) {
        // if not acked, promulagates error
        if (tag.isSelfClosing) {
          tokeniser.acknowledgeSelfClosingFlag()
        }
      } else {
        // unknown tag, remember this is self closing for output
        tag.setSelfClosing()
        tokeniser.acknowledgeSelfClosingFlag()
      }
    }
    return el
  }

  private[parser] def insertForm(startTag: Token.StartTag, onStack: Boolean): FormElement = {
    val tag: Tag = Tag(startTag.name)
    val el: FormElement = new FormElement(tag, baseUri, startTag.attributes)
    setFormElement(el)
    insertNode(el)
    if (onStack) {
      stack.append(el)
    }
    return el
  }

  private[parser] def insert(commentToken: Token.Comment) {
    val comment: Comment = new Comment(commentToken.getData, baseUri)
    insertNode(comment)
  }

  private[parser] def insert(characterToken: Token.Character) {
    var node: Node = null
    // characters in script and style go in as datanodes, not text nodes
    val tagName: String = currentElement.tagName
    if ((tagName == "script") || (tagName == "style")) {
      node = new DataNode(characterToken.getData, baseUri)
    } else {
      node = new TextNode(characterToken.getData, baseUri)
    }
    // doesn't use insertNode, because we don't foster these; and will always have a stack.
    currentElement.appendChild(node)
  }

  private def insertNode(node: Node) {
    // if the stack hasn't been set up yet, elements (doctype, comments) go into the doc
    if (stack.size == 0) {
      doc.appendChild(node)
    } else if (isFosterInserts) {
      insertInFosterParent(node)
    } else {
      currentElement.appendChild(node)
    }
    // connect form controls to their form element
    if (node.isInstanceOf[Element] && (node.asInstanceOf[Element]).tag.isFormListed) {
      if (formElement != null) {
        formElement.addElement(node.asInstanceOf[Element])
      }
    }
  }

  private[parser] def pop: Element = {
    val size: Int = stack.size
    return stack.remove(size - 1)
  }

  private[parser] def push(element: Element) {
    stack.append(element)
  }

  private[parser] def getStack: mutable.Buffer[Element] = {
    return stack
  }

  private[parser] def onStack(el: Element): Boolean = {
    return isElementInQueue(stack, el)
  }

  private def isElementInQueue(queue: mutable.Buffer[Element], element: Element): Boolean = {
    for (pos <- (queue.size - 1).to(0, -1)) {
      val next: Element = queue(pos)
      if (next eq element) {
        return true
      }
    }
    false
  }

  private[parser] def getFromStack(elName: String): Element = {
    for (pos <- (stack.size - 1).to(0, -1)) {
      val next: Element = stack(pos)
      if (next.nodeName == elName) {
        return next
      }
    }
    return null
  }

  private[parser] def removeFromStack(el: Element): Boolean = {
    for (pos <- (stack.size - 1).to(0, -1)) {
      val next: Element = stack(pos)
      if (next eq el) {
        stack.remove(pos)
        return true
      }
    }
    return false
  }

  private[parser] def popStackToClose(elName: String) {
    breakable {
      for (pos <- (stack.size - 1).to(0, -1)) {
        val next: Element = stack(pos)
        stack.remove(pos)
        if (next.nodeName == elName) {
          break()
        }
      }
    }
  }

  private[parser] def popStackToClose(elNames: String*) {
    breakable {
      for (pos <- (stack.size - 1).to(0, -1)) {
        val next: Element = stack(pos)
        stack.remove(pos)
        if (Strings.in(next.nodeName(), elNames: _*)) {
          break()
        }
      }
    }
  }

  private[parser] def popStackToBefore(elName: String) {
    breakable {
      for (pos <- (stack.size - 1).to(0, -1)) {
        val next: Element = stack(pos)
        if (next.nodeName == elName) {
          break()
        } else {
          stack.remove(pos)
        }
      }
    }
  }

  private[parser] def clearStackToTableContext {
    clearStackToContext("table")
  }

  private[parser] def clearStackToTableBodyContext {
    clearStackToContext("tbody", "tfoot", "thead")
  }

  private[parser] def clearStackToTableRowContext {
    clearStackToContext("tr")
  }

  private def clearStackToContext(nodeNames: String*) {
    breakable {
      for (pos <- (stack.size - 1).to(0, -1)) {
        val next: Element = stack(pos)
        if (Strings.in(next.nodeName(), nodeNames: _*) || (next.nodeName() == "html")) {
          break()
        } else {
          stack.remove(pos)
        }
      }
    }
  }

  private[parser] def aboveOnStack(el: Element): Element = {
    assert(onStack(el))
    breakable {
      for (pos <- (stack.size - 1).to(0, -1)) {
        val next: Element = stack(pos)
        if (next eq el) {
          return stack(pos - 1)
        }
      }
    }
    return null
  }

  private[parser] def insertOnStackAfter(after: Element, in: Element) {
    val i: Int = stack.lastIndexOf(after)
    Validator.isTrue(i != -1)
    stack.insert(i + 1, in)
  }

  private[parser] def replaceOnStack(out: Element, in: Element) {
    replaceInQueue(stack, out, in)
  }

  private def replaceInQueue(queue: mutable.Buffer[Element], out: Element, in: Element) {
    val i: Int = queue.lastIndexOf(out)
    Validator.isTrue(i != -1)
    queue.update(i, in)
  }

  override protected def process(token: Token): Boolean = ???
}

private[parser] object HtmlTreeBuilder {

  private[HtmlTreeBuilder] val TagsScriptStyle: Array[String] = Array[String]("script", "style")
  val TagsSearchInScope: Array[String] = Array[String]("applet", "caption", "html", "table", "td", "th", "marquee", "object")
  private[HtmlTreeBuilder] val TagSearchList: Array[String] = Array[String]("ol", "ul")
  private[HtmlTreeBuilder] val TagSearchButton: Array[String] = Array[String]("button")
  private[HtmlTreeBuilder] val TagSearchTableScope: Array[String] = Array[String]("html", "table")
  private[HtmlTreeBuilder] val TagSearchSelectScope: Array[String] = Array[String]("optgroup", "option")
  private[HtmlTreeBuilder] val TagSearchEndTags: Array[String] = Array[String]("dd", "dt", "li", "option", "optgroup", "p", "rp", "rt")
  private[HtmlTreeBuilder] val TagSearchSpecial: Array[String] = Array[String]("address", "applet", "area", "article", "aside", "base", "basefont", "bgsound", "blockquote", "body", "br", "button", "caption", "center", "col", "colgroup", "command", "dd", "details", "dir", "div", "dl", "dt", "embed", "fieldset", "figcaption", "figure", "footer", "form", "frame", "frameset", "h1", "h2", "h3", "h4", "h5", "h6", "head", "header", "hgroup", "hr", "html", "iframe", "img", "input", "isindex", "li", "link", "listing", "marquee", "menu", "meta", "nav", "noembed", "noframes", "noscript", "object", "ol", "p", "param", "plaintext", "pre", "script", "section", "select", "style", "summary", "table", "tbody", "td", "textarea", "tfoot", "th", "thead", "title", "tr", "ul", "wbr", "xmp")
}