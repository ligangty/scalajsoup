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
    // context may be null
    stateVal = HtmlTreeBuilderState.Initial
    initialiseParse(inputFragment, baseUri, errors)
    contextElement = context
    fragmentParsing = true
    var root: Element = null
    if (context != null) {
      if (context.ownerDocument != null) {
        // quirks setup:
        doc.quirksMode(context.ownerDocument.quirksMode())
      }
      // initialise the tokeniser state:
      val contextTag: String = context.tagName
      if (Strings.in(contextTag, "title", "textarea")) {
        tokeniser.transition(TokeniserState.Rcdata)
      } else if (Strings.in(contextTag, "iframe", "noembed", "noframes", "style", "xmp")) {
        tokeniser.transition(TokeniserState.Rawtext)
      } else if (contextTag == "script") {
        tokeniser.transition(TokeniserState.ScriptData)
      } else if (contextTag == "noscript") {
        tokeniser.transition(TokeniserState.Data) // if scripting enabled, rawtext
      } else if (contextTag == "plaintext") {
        tokeniser.transition(TokeniserState.Data)
      } else {
        tokeniser.transition(TokeniserState.Data) // default
      }

      root = new Element(Tag("html"), baseUri)
      doc.appendChild(root)
      stack.append(root)
      resetInsertionMode()

      // setup form element to nearest form on context (up ancestor chain). ensures form controls are associated
      // with form correctly
      val contextChain: Elements = context.parents
      contextChain.add(0, context)
      breakable {
        for (parent <- contextChain) {
          parent match {
            case fe: FormElement =>
              formElement = fe
              break()
            case _ =>
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

  override protected[parser] def process(token: Token): Boolean = {
    currentToken = token
    this.stateVal.process(token, this)
  }

  private[parser] def process(token: Token, state: HtmlTreeBuilderState.BuilderState): Boolean = {
    currentToken = token
    state.process(token, this)
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
    originalStateVal
  }

  private[parser] def framesetOk(framesetOk: Boolean) {
    this.framesetOkVal = framesetOk
  }

  private[parser] def framesetOk: Boolean = {
    framesetOkVal
  }

  private[parser] def getDocument: Document = {
    doc
  }

  private[parser] def getBaseUri: String = {
    baseUri
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
    fragmentParsing
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
      el
    }
    val el: Element = new Element(Tag(startTag.name), baseUri, startTag.attributes)
    insert(el)
    el
  }

  private[parser] def insertStartTag(startTagName: String): Element = {
    val el: Element = new Element(Tag(startTagName), baseUri)
    insert(el)
    el
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
    el
  }

  private[parser] def insertForm(startTag: Token.StartTag, onStack: Boolean): FormElement = {
    val tag: Tag = Tag(startTag.name)
    val el: FormElement = new FormElement(tag, baseUri, startTag.attributes)
    setFormElement(el)
    insertNode(el)
    if (onStack) {
      stack.append(el)
    }
    el
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
    node match {
      case e: Element if e.tag.isFormListed =>
        if (formElement != null) formElement.addElement(node.asInstanceOf[Element])
      case _ =>
    }
  }

  private[parser] def pop: Element = {
    val size: Int = stack.size
    stack.remove(size - 1)
  }

  private[parser] def push(element: Element) {
    stack.append(element)
  }

  private[parser] def getStack: mutable.Buffer[Element] = {
    stack
  }

  private[parser] def onStack(el: Element): Boolean = {
    isElementInQueue(stack, el)
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
    null
  }

  private[parser] def removeFromStack(el: Element): Boolean = {
    for (pos <- (stack.size - 1).to(0, -1)) {
      val next: Element = stack(pos)
      if (next eq el) {
        stack.remove(pos)
        return true
      }
    }
    false
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

  private[parser] def clearStackToTableContext(): Unit = {
    clearStackToContext("table")
  }

  private[parser] def clearStackToTableBodyContext(): Unit = {
    clearStackToContext("tbody", "tfoot", "thead")
  }

  private[parser] def clearStackToTableRowContext(): Unit = {
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
    null
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

  private[parser] def resetInsertionMode(): Unit = {
    var last: Boolean = false
    breakable {
      for (pos <- (stack.size - 1).to(0, -1)) {
        var node: Element = stack(pos)
        if (pos == 0) {
          last = true
          node = contextElement
        }
        val name: String = node.nodeName()
        if ("select" == name) {
          transition(HtmlTreeBuilderState.InSelect)
          break() // frag
        } else if ("td" == name || ("td" == name) && !last) {
          transition(HtmlTreeBuilderState.InCell)
          break()
        } else if ("tr" == name) {
          transition(HtmlTreeBuilderState.InRow)
          break()
        } else if (("tbody" == name) || ("thead" == name) || ("tfoot" == name)) {
          transition(HtmlTreeBuilderState.InTableBody)
          break()
        } else if ("caption" == name) {
          transition(HtmlTreeBuilderState.InCaption)
          break()
        } else if ("colgroup" == name) {
          transition(HtmlTreeBuilderState.InColumnGroup)
          break() // frag
        } else if ("table" == name) {
          transition(HtmlTreeBuilderState.InTable)
          break()
        } else if ("head" == name) {
          transition(HtmlTreeBuilderState.InBody)
          break() // frag
        } else if ("body" == name) {
          transition(HtmlTreeBuilderState.InBody)
          break()
        } else if ("frameset" == name) {
          transition(HtmlTreeBuilderState.InFrameset)
          break() // frag
        } else if ("html" == name) {
          transition(HtmlTreeBuilderState.BeforeHead)
          break() // frag
        } else if (last) {
          transition(HtmlTreeBuilderState.InBody)
          break() // frag
        }
      }
    }
  }

  // todo: tidy up in specific scope methods
  private var specificScopeTarget: Array[String] = Array(null)

  private def inSpecificScope(targetName: String, baseTypes: Array[String], extraTypes: Array[String]): Boolean = {
    specificScopeTarget(0) = targetName
    inSpecificScope(specificScopeTarget, baseTypes, extraTypes)
  }

  private def inSpecificScope(targetNames: Array[String], baseTypes: Array[String], extraTypes: Array[String]): Boolean = {
    for (pos <- (stack.size - 1).to(0, -1)) {
      val el: Element = stack(pos)
      val elName: String = el.nodeName()
      if (Strings.in(elName, targetNames: _*)) return true
      if (Strings.in(elName, baseTypes: _*)) return false
      if (extraTypes != null && Strings.in(elName, extraTypes: _*)) return false
    }
    Validator.fail("Should not be reachable")
    false
  }

  private[parser] def inScope(targetNames: Array[String]): Boolean = {
    inSpecificScope(targetNames, HtmlTreeBuilder.TagsSearchInScope, null)
  }

  private[parser] def inScope(targetName: String): Boolean = {
    inScope(targetName, null)
  }

  private[parser] def inScope(targetName: String, extras: Array[String]): Boolean = {
    inSpecificScope(targetName, HtmlTreeBuilder.TagsSearchInScope, extras)
    // todo: in mathml namespace: mi, mo, mn, ms, mtext annotation-xml
    // todo: in svg namespace: forignOjbect, desc, title
  }

  private[parser] def inListItemScope(targetName: String): Boolean = {
    inScope(targetName, HtmlTreeBuilder.TagSearchList)
  }

  private[parser] def inButtonScope(targetName: String): Boolean = {
    inScope(targetName, HtmlTreeBuilder.TagSearchButton)
  }

  private[parser] def inTableScope(targetName: String): Boolean = {
    inSpecificScope(targetName, HtmlTreeBuilder.TagSearchTableScope, null)
  }

  private[parser] def inSelectScope(targetName: String): Boolean = {
    for (pos <- (stack.size - 1).to(0, -1)) {
      val el: Element = stack(pos)
      val elName: String = el.nodeName()
      if (elName == targetName) return true
      if (!Strings.in(elName, HtmlTreeBuilder.TagSearchSelectScope: _*)) return false
    }
    Validator.fail("Should not be reachable")
    false
  }

  private[parser] def setHeadElement(headElement: Element) {
    this.headElement = headElement
  }

  private[parser] def getHeadElement: Element = {
    headElement
  }

  private[parser] def isFosterInserts: Boolean = {
    fosterInserts
  }

  private[parser] def setFosterInserts(fosterInserts: Boolean) {
    this.fosterInserts = fosterInserts
  }

  private[parser] def getFormElement: FormElement = {
    formElement
  }

  private[parser] def setFormElement(formElement: FormElement) {
    this.formElement = formElement
  }

  private[parser] def newPendingTableCharacters(): Unit = {
    pendingTableCharacters = new mutable.ArrayBuffer[String]
  }

  private[parser] def getPendingTableCharacters: mutable.Buffer[String] = {
    pendingTableCharacters
  }

  private[parser] def setPendingTableCharacters(pendingTableCharacters: mutable.Buffer[String]) {
    this.pendingTableCharacters = pendingTableCharacters
  }

  /**
   * 11.2.5.2 Closing elements that have implied end tags<p/>
     When the steps below require the UA to generate implied end tags, then, while the current node is a dd element, a
     dt element, an li element, an option element, an optgroup element, a p element, an rp element, or an rt element,
     the UA must pop the current node off the stack of open elements.

     @param excludeTag If a step requires the UA to generate implied end tags but lists an element to exclude from the
     process, then the UA must perform the above steps as if that element was not in the above list.
   */
  private[parser] def generateImpliedEndTags(excludeTag: String) {
    while ((excludeTag != null && !(currentElement.nodeName == excludeTag)) && Strings.in(currentElement.nodeName(), HtmlTreeBuilder.TagSearchEndTags: _*)) pop
  }

  private[parser] def generateImpliedEndTags(): Unit = {
    generateImpliedEndTags(null)
  }

  private[parser] def isSpecial(el: Element): Boolean = {
    // todo: mathml's mi, mo, mn
    // todo: svg's foreigObject, desc, title
    val name: String = el.nodeName()
    Strings.in(name, HtmlTreeBuilder.TagSearchSpecial: _*)
  }

  private[parser] def lastFormattingElement: Element = {
    if (formattingElements.size > 0) formattingElements(formattingElements.size - 1) else null
  }

  private[parser] def removeLastFormattingElement(): Element = {
    val size: Int = formattingElements.size
    if (size > 0) formattingElements.remove(size - 1)
    else null
  }

  // active formatting elements
  private[parser] def pushActiveFormattingElements(in: Element) {
    var numSeen: Int = 0
    breakable {
      for (pos <- (formattingElements.size - 1).to(0, -1)) {
        val el: Element = formattingElements(pos)
        if (el == null) break() // mark

        if (isSameFormattingElement(in, el))
          numSeen += 1

        if (numSeen == 3) {
          formattingElements.remove(pos)
          break()
        }
      }
    }
    formattingElements.append(in)
  }

  private def isSameFormattingElement(a: Element, b: Element): Boolean = {
    // same if: same namespace, tag, and attributes. Element.equals only checks tag, might in future check children
    (a.nodeName == b.nodeName) && (a.attributes == b.attributes)
    // && a.namespace().equals(b.namespace())
    // todo: namespaces
  }

  private[parser] def reconstructFormattingElements(): Unit = {
    val last: Element = lastFormattingElement
    if (last == null || onStack(last)) return
    var entry: Element = last
    val size: Int = formattingElements.size
    var pos: Int = size - 1
    var skip: Boolean = false
    breakable {
      while (true) {
        if (pos == 0) {
          // step 4. if none before, skip to 8
          skip = true
          break()
        }
        pos -= 1
        entry = formattingElements(pos) // step 5. one earlier than entry
        if (entry == null || onStack(entry)) // step 6 - neither marker nor on stack
          break() // jump to 8, else continue back to 4
      }
    }
    breakable {
      while (true) {
        if (!skip) {
          // step 7: on later than entry
          pos += 1
          entry = formattingElements(pos)
        }
        Validator.notNull(entry) // should not occur, as we break at last element
        // 8. create new element from element, 9 insert into current node, onto stack
        skip = false // can only skip increment from 4.
        val newEl: Element = insertStartTag(entry.nodeName()) // todo: avoid fostering here?
        // newEl.namespace(entry.namespace()) // todo: namespaces
        newEl.attributes.addAll(entry.attributes)
        // 10. replace entry with new entry
        formattingElements.update(pos, newEl)
        // 11
        if (pos == size - 1) // if not last entry in list, jump to 7
          break()
      }
    }
  }

  private[parser] def clearFormattingElementsToLastMarker(): Unit = {
    breakable {
      while (formattingElements.nonEmpty) {
        val el: Element = removeLastFormattingElement()
        if (el == null) break()
      }
    }
  }

  private[parser] def removeFromActiveFormattingElements(el: Element) {
    breakable {
      for (pos <- (formattingElements.size - 1).to(0, -1)) {
        val next: Element = formattingElements(pos)
        if (next eq el) {
          formattingElements.remove(pos)
          break()
        }
      }
    }
  }

  private[parser] def isInActiveFormattingElements(el: Element): Boolean = {
    isElementInQueue(formattingElements, el)
  }

  private[parser] def getActiveFormattingElement(nodeName: String): Element = {
    breakable {
      for (pos <- (formattingElements.size - 1).to(0, -1)) {
        val next: Element = formattingElements(pos)
        if (next == null) break()
        else if (next.nodeName == nodeName) return next
      }
    }
    null
  }

  private[parser] def replaceActiveFormattingElement(out: Element, in: Element) {
    replaceInQueue(formattingElements, out, in)
  }

  private[parser] def insertMarkerToFormattingElements(): Unit = {
    formattingElements.append(null)
  }

  private[parser] def insertInFosterParent(in: Node) {
    var fosterParent: Element = null
    val lastTable: Element = getFromStack("table")
    var isLastTableParent: Boolean = false
    if (lastTable != null) {
      if (lastTable.parent != null) {
        fosterParent = lastTable.parent
        isLastTableParent = true
      } else fosterParent = aboveOnStack(lastTable)
    } else {
      fosterParent = stack.head
    }
    if (isLastTableParent) {
      Validator.notNull(lastTable)
      lastTable.before(in)
    } else fosterParent.appendChild(in)
  }

  override def toString: String = {
    "TreeBuilder{" + "currentToken=" + currentToken + ", state=" + state + ", currentElement=" + currentElement + '}'
  }

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