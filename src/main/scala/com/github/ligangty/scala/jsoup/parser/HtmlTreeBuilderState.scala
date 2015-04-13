package com.github.ligangty.scala.jsoup.parser

import com.github.ligangty.scala.jsoup.helper.Strings
import com.github.ligangty.scala.jsoup.nodes._
import Token.TokenType._
import com.github.ligangty.scala.jsoup.parser.Token.TokenType.Comment
import com.github.ligangty.scala.jsoup.parser.TokeniserState.{Rawtext, Rcdata}

import scala.collection.mutable
import scala.util.control.Breaks
import scala.util.control.Breaks._

/**
 * The Tree Builder's current state. Each state embodies the processing for the state, and transitions to other states.
 */
private[parser] object HtmlTreeBuilderState {

  private[parser] sealed trait BuilderState {

    private[parser] def process(t: Token, tb: HtmlTreeBuilder): Boolean
  }

  private[parser] case object Initial extends BuilderState {

    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Boolean = {
      if (isWhitespace(t)) {
        return true
      } else if (t.isComment) {
        tb.insert(t.asComment)
      } else if (t.isDoctype) {
        val d: Token.Doctype = t.asDoctype
        val doctype: DocumentType = new DocumentType(d.getName, d.getPublicIdentifier, d.getSystemIdentifier, tb.getBaseUri)
        tb.getDocument.appendChild(doctype)
        if (d.isForceQuirks) {
          tb.getDocument.quirksMode(Document.QuirksMode.quirks)
        }
        tb.transition(BeforeHtml)
      } else {
        tb.transition(BeforeHtml)
        return tb.process(t)
      }
      true
    }
  }

  private[parser] case object BeforeHtml extends BuilderState {

    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Boolean = {
      if (t.isDoctype) {
        tb.error(this)
        return false
      } else if (t.isComment) {
        tb.insert(t.asComment)
      } else if (isWhitespace(t)) {
        return true
      } else if (t.isStartTag && (t.asStartTag.name == "html")) {
        tb.insert(t.asStartTag)
        tb.transition(BeforeHead)
      } else if (t.isEndTag && Strings.in(t.asEndTag.name, "head", "body", "html", "br")) {
        return anythingElse(t, tb)
      } else if (t.isEndTag) {
        tb.error(this)
        return false
      } else {
        return anythingElse(t, tb)
      }
      true
    }

    private def anythingElse(t: Token, tb: HtmlTreeBuilder): Boolean = {
      tb.insertStartTag("html")
      tb.transition(BeforeHead)
      tb.process(t)
    }
  }

  private[parser] case object BeforeHead extends BuilderState {

    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Boolean = {
      if (isWhitespace(t)) {
        return true
      } else if (t.isComment) {
        tb.insert(t.asComment)
      } else if (t.isDoctype) {
        tb.error(this)
        return false
      } else if (t.isStartTag && (t.asStartTag.name == "html")) {
        return InBody.process(t, tb)
      } else if (t.isStartTag && (t.asStartTag.name == "head")) {
        val head: Element = tb.insert(t.asStartTag)
        tb.setHeadElement(head)
        tb.transition(InHead)
      } else if (t.isEndTag && Strings.in(t.asEndTag.name, "head", "body", "html", "br")) {
        tb.processStartTag("head")
        return tb.process(t)
      } else if (t.isEndTag) {
        tb.error(this)
        return false
      } else {
        tb.processStartTag("head")
        return tb.process(t)
      }
      true
    }
  }

  private[parser] case object InHead extends BuilderState {

    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Boolean = {
      if (isWhitespace(t)) {
        tb.insert(t.asCharacter)
        return true
      }
      t.tokType match {
        case Comment =>
          tb.insert(t.asComment)
        case Doctype =>
          tb.error(this)
          return false
        case StartTag =>
          val start: Token.StartTag = t.asStartTag
          val name: String = start.name
          if (name == "html") {
            return InBody.process(t, tb)
          } else if (Strings.in(name, "base", "basefont", "bgsound", "command", "link")) {
            val el: Element = tb.insertEmpty(start)
            if ((name == "base") && el.hasAttr("href")) {
              tb.maybeSetBaseUri(el)
            }
          } else if (name == "meta") {
            val meta: Element = tb.insertEmpty(start)
          } else if (name == "title") {
            handleRcData(start, tb)
          } else if (Strings.in(name, "noframes", "style")) {
            handleRawtext(start, tb)
          } else if (name == "noscript") {
            tb.insert(start)
            tb.transition(InHeadNoscript)
          } else if (name == "script") {
            tb.tokeniser.transition(TokeniserState.ScriptData)
            tb.markInsertionMode()
            tb.transition(Text)
            tb.insert(start)
          } else if (name == "head") {
            tb.error(this)
            return false
          } else {
            return anythingElse(t, tb)
          }
        case EndTag =>
          val end: Token.EndTag = t.asEndTag
          val name = end.name
          if (name == "head") {
            tb.pop
            tb.transition(AfterHead)
          } else if (Strings.in(name, "body", "html", "br")) {
            return anythingElse(t, tb)
          } else {
            tb.error(this)
            return false
          }
        case _ =>
          return anythingElse(t, tb)
      }
      true
    }

    private def anythingElse(t: Token, tb: TreeBuilder): Boolean = {
      tb.processEndTag("head")
      tb.process(t)
    }
  }

  private[parser] case object InHeadNoscript extends BuilderState {

    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Boolean = {
      if (t.isDoctype) {
        tb.error(this)
      } else if (t.isStartTag && (t.asStartTag.name == "html")) {
        return tb.process(t, InBody)
      } else if (t.isEndTag && (t.asEndTag.name == "noscript")) {
        tb.pop
        tb.transition(InHead)
      } else if (isWhitespace(t) || t.isComment || (t.isStartTag && Strings.in(t.asStartTag.name, "basefont", "bgsound", "link", "meta", "noframes", "style"))) {
        return tb.process(t, InHead)
      } else if (t.isEndTag && (t.asEndTag.name == "br")) {
        return anythingElse(t, tb)
      } else if ((t.isStartTag && Strings.in(t.asStartTag.name, "head", "noscript")) || t.isEndTag) {
        tb.error(this)
        return false
      } else {
        return anythingElse(t, tb)
      }
      true
    }

    private def anythingElse(t: Token, tb: HtmlTreeBuilder): Boolean = {
      tb.error(this)
      tb.processEndTag("noscript")
      tb.process(t)
    }
  }

  private[parser] case object AfterHead extends BuilderState {

    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Boolean = {
      if (isWhitespace(t)) {
        tb.insert(t.asCharacter)
      } else if (t.isComment) {
        tb.insert(t.asComment)
      } else if (t.isDoctype) {
        tb.error(this)
      } else if (t.isStartTag) {
        val startTag: Token.StartTag = t.asStartTag
        val name: String = startTag.name
        if (name == "html") {
          return tb.process(t, InBody)
        } else if (name == "body") {
          tb.insert(startTag)
          tb.framesetOk(false)
          tb.transition(InBody)
        } else if (name == "frameset") {
          tb.insert(startTag)
          tb.transition(InFrameset)
        } else if (Strings.in(name, "base", "basefont", "bgsound", "link", "meta", "noframes", "script", "style", "title")) {
          tb.error(this)
          val head: Element = tb.getHeadElement
          tb.push(head)
          tb.process(t, InHead)
          tb.removeFromStack(head)
        } else if (name == "head") {
          tb.error(this)
          return false
        } else {
          anythingElse(t, tb)
        }
      } else if (t.isEndTag) {
        if (Strings.in(t.asEndTag.name, "body", "html")) {
          anythingElse(t, tb)
        } else {
          tb.error(this)
          return false
        }
      } else {
        anythingElse(t, tb)
      }
      true
    }

    private def anythingElse(t: Token, tb: HtmlTreeBuilder): Boolean = {
      tb.processStartTag("body")
      tb.framesetOk(true)
      tb.process(t)
    }
  }

  private[parser] case object InBody extends BuilderState {

    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Boolean = {
      t.tokType match {
        case Character =>
          val c: Token.Character = t.asCharacter
          if (c.getData == nullString) {
            // todo confirm that check
            tb.error(this)
            return false
          } else if (tb.framesetOk && isWhitespace(c)) {
            // don't check if whitespace if frames already closed
            tb.reconstructFormattingElements()
            tb.insert(c)
          } else {
            tb.reconstructFormattingElements()
            tb.insert(c)
            tb.framesetOk(false)
          }
        case Comment =>
          tb.insert(t.asComment)
        case Doctype =>
          tb.error(this)
          return false
        case StartTag =>
          val startTag: Token.StartTag = t.asStartTag
          var name: String = startTag.name
          if (name == "html") {
            tb.error(this)
            // merge attributes onto real html
            val html: Element = tb.getStack.head
            for (attribute <- startTag.getAttributes if !html.hasAttr(attribute.getKey)) {
              html.attributes.put(attribute)
            }
          } else if (Strings.in(name, Constants.InBodyStartToHead: _*)) {
            return tb.process(t, InHead)
          } else if (name == "body") {
            tb.error(this)
            val stack: mutable.Buffer[Element] = tb.getStack
            if (stack.size == 1 || (stack.size > 2 && stack(1).nodeName() != "body")) {
              // only in fragment case
              return false // ignore
            } else {
              tb.framesetOk(false)
              val body: Element = stack(1)
              for (attribute <- startTag.getAttributes if !body.hasAttr(attribute.getKey)) {
                body.attributes.put(attribute)
              }
            }
          } else if (name == "frameset") {
            tb.error(this)
            val stack: mutable.Buffer[Element] = tb.getStack
            if (stack.size == 1 || (stack.size > 2 && stack(1).nodeName != "body")) {
              // only in fragment case
              return false // ignore
            } else if (!tb.framesetOk) {
              return false // ignore frameset
            } else {
              val second: Element = stack(1)
              if (second.parent != null) {
                second.remove()
              }
              // pop up to html element
              while (stack.size > 1) {
                stack.remove(stack.size - 1)
              }
              tb.insert(startTag)
              tb.transition(InFrameset)
            }
          } else if (Strings.in(name, Constants.InBodyStartPClosers: _*)) {
            if (tb.inButtonScope("p")) {
              tb.processEndTag("p")
            }
            tb.insert(startTag)
          } else if (Strings.in(name, Constants.Headings: _*)) {
            if (tb.inButtonScope("p")) {
              tb.processEndTag("p")
            }
            if (Strings.in(tb.currentElement.nodeName(), Constants.Headings: _*)) {
              tb.error(this)
              tb.pop
            }
            tb.insert(startTag)
          } else if (Strings.in(name, Constants.InBodyStartPreListing: _*)) {
            if (tb.inButtonScope("p")) {
              tb.processEndTag("p")
            }
            tb.insert(startTag)
            // todo: ignore LF if next token
            tb.framesetOk(false)
          } else if (name == "form") {
            if (tb.getFormElement != null) {
              tb.error(this)
              return false
            }
            if (tb.inButtonScope("p")) {
              tb.processEndTag("p")
            }
            tb.insertForm(startTag, true)
          } else if (name == "li") {
            tb.framesetOk(false)
            val stack: mutable.Buffer[Element] = tb.getStack
            breakable {
              for (i <- (stack.size - 1).to(1, -1)) {
                val el: Element = stack(i)
                if (el.nodeName == "li") {
                  tb.processEndTag("li")
                  break()
                }
                if (tb.isSpecial(el) && !Strings.in(el.nodeName(), Constants.InBodyStartLiBreakers: _*)) {
                  break()
                }
              }
            }
            if (tb.inButtonScope("p")) {
              tb.processEndTag("p")
            }
            tb.insert(startTag)
          } else if (Strings.in(name, Constants.DdDt: _*)) {
            tb.framesetOk(false)
            val stack: mutable.Buffer[Element] = tb.getStack
            breakable {
              for (i <- (stack.size - 1).to(1, -1)) {
                val el: Element = stack(i)
                if (Strings.in(el.nodeName(), Constants.DdDt: _*)) {
                  tb.processEndTag(el.nodeName())
                  break()
                }
                if (tb.isSpecial(el) && !Strings.in(el.nodeName(), Constants.InBodyStartLiBreakers: _*)) {
                  break()
                }
              }
            }
            if (tb.inButtonScope("p")) {
              tb.processEndTag("p")
            }
            tb.insert(startTag)
          } else if (name == "plaintext") {
            if (tb.inButtonScope("p")) {
              tb.processEndTag("p")
            }
            tb.insert(startTag)
            // once in, never gets out
            tb.tokeniser.transition(TokeniserState.PLAINTEXT)
          } else if (name == "button") {
            if (tb.inButtonScope("button")) {
              // close and reprocess
              tb.error(this)
              tb.processEndTag("button")
              tb.process(startTag)
            } else {
              tb.reconstructFormattingElements()
              tb.insert(startTag)
              tb.framesetOk(false)
            }
          } else if (name == "a") {
            if (tb.getActiveFormattingElement("a") != null) {
              tb.error(this)
              tb.processEndTag("a")
              // still on stack?
              val remainingA: Element = tb.getFromStack("a")
              if (remainingA != null) {
                tb.removeFromActiveFormattingElements(remainingA)
                tb.removeFromStack(remainingA)
              }
            }
            tb.reconstructFormattingElements()
            val a: Element = tb.insert(startTag)
            tb.pushActiveFormattingElements(a)
          } else if (Strings.in(name, Constants.Formatters: _*)) {
            tb.reconstructFormattingElements()
            val el: Element = tb.insert(startTag)
            tb.pushActiveFormattingElements(el)
          } else if (name == "nobr") {
            tb.reconstructFormattingElements()
            if (tb.inScope("nobr")) {
              tb.error(this)
              tb.processEndTag("nobr")
              tb.reconstructFormattingElements()
            }
            val el: Element = tb.insert(startTag)
            tb.pushActiveFormattingElements(el)
          } else if (Strings.in(name, Constants.InBodyStartApplets: _*)) {
            tb.reconstructFormattingElements()
            tb.insert(startTag)
            tb.insertMarkerToFormattingElements()
            tb.framesetOk(false)
          } else if (name == "table") {
            if ((tb.getDocument.quirksMode ne Document.QuirksMode.quirks) && tb.inButtonScope("p")) {
              tb.processEndTag("p")
            }
            tb.insert(startTag)
            tb.framesetOk(false)
            tb.transition(InTable)
          } else if (Strings.in(name, Constants.InBodyStartEmptyFormatters: _*)) {
            tb.reconstructFormattingElements()
            tb.insertEmpty(startTag)
            tb.framesetOk(false)
          } else if (name == "input") {
            tb.reconstructFormattingElements()
            val el: Element = tb.insertEmpty(startTag)
            if (!el.attr("type").equalsIgnoreCase("hidden")) {
              tb.framesetOk(false)
            }
          } else if (Strings.in(name, Constants.InBodyStartMedia: _*)) {
            tb.insertEmpty(startTag)
          } else if (name == "hr") {
            if (tb.inButtonScope("p")) {
              tb.processEndTag("p")
            }
            tb.insertEmpty(startTag)
            tb.framesetOk(false)
          } else if (name == "image") {
            // change <image> to <img>, unless in svg
            if (tb.getFromStack("svg") == null) {
              return tb.process(startTag.name("img"))
            }
            else {
              tb.insert(startTag)
            }
          } else if (name == "isindex") {
            // how much do we care about the early 90s?
            tb.error(this)
            if (tb.getFormElement != null) {
              return false
            }
            tb.tokeniser.acknowledgeSelfClosingFlag()
            tb.processStartTag("form")
            if (startTag.attributes.hasKey("action")) {
              val form: Element = tb.getFormElement
              form.attr("action", startTag.attributes.get("action"))
            }
            tb.processStartTag("hr")
            tb.processStartTag("label")
            // hope you like english.
            val prompt: String = {
              if (startTag.attributes.hasKey("prompt")) {
                startTag.attributes.get("prompt")
              }
              else {
                "This is a searchable index. Enter search keywords: "
              }
            }
            tb.process(new Token.Character().data(prompt))
            // input
            val inputAttribs: Attributes = new Attributes
            for (attr <- startTag.attributes if !Strings.in(attr.getKey, Constants.InBodyStartInputAttribs: _*)) {
              inputAttribs.put(attr)
            }
            inputAttribs.put("name", "isindex")
            tb.processStartTag("input", inputAttribs)
            tb.processEndTag("label")
            tb.processStartTag("hr")
            tb.processEndTag("form")
          } else if (name == "textarea") {
            tb.insert(startTag)
            // todo: If the next token is a U+000A LINE FEED (LF) character token, then ignore that token and move on to the next one. (Newlines at the start of textarea elements are ignored as an authoring convenience.)
            tb.tokeniser.transition(TokeniserState.Rcdata)
            tb.markInsertionMode()
            tb.framesetOk(false)
            tb.transition(Text)
          } else if (name == "xmp") {
            if (tb.inButtonScope("p")) {
              tb.processEndTag("p")
            }
            tb.reconstructFormattingElements()
            tb.framesetOk(false)
            handleRawtext(startTag, tb)
          } else if (name == "iframe") {
            tb.framesetOk(false)
            handleRawtext(startTag, tb)
          } else if (name == "noembed") {
            // also handle noscript if script enabled
            handleRawtext(startTag, tb)
          } else if (name == "select") {
            tb.reconstructFormattingElements()
            tb.insert(startTag)
            tb.framesetOk(false)
            val state: HtmlTreeBuilderState.BuilderState = tb.state
            if ((state == InTable) || (state == InCaption) || (state == InTableBody) || (state == InRow) || (state == InCell)) {
              tb.transition(InSelectInTable)
            }
            else {
              tb.transition(InSelect)
            }
          } else if (Strings.in(name, Constants.InBodyStartOptions: _*)) {
            if (tb.currentElement.nodeName == "option") {
              tb.processEndTag("option")
            }
            tb.reconstructFormattingElements()
            tb.insert(startTag)
          } else if (Strings.in(name, Constants.InBodyStartRuby: _*)) {
            if (tb.inScope("ruby")) {
              tb.generateImpliedEndTags()
              if (!(tb.currentElement.nodeName == "ruby")) {
                tb.error(this)
                tb.popStackToBefore("ruby") // i.e. close up to but not include name
              }
              tb.insert(startTag)
            }
          } else if (name == "math") {
            tb.reconstructFormattingElements()
            // todo: handle A start tag whose tag name is "math" (i.e. foreign, mathml)
            tb.insert(startTag)
            tb.tokeniser.acknowledgeSelfClosingFlag()
          } else if (name == "svg") {
            tb.reconstructFormattingElements()
            // todo: handle A start tag whose tag name is "svg" (xlink, svg)
            tb.insert(startTag)
            tb.tokeniser.acknowledgeSelfClosingFlag()
          } else if (Strings.in(name, Constants.InBodyStartDrop: _*)) {
            tb.error(this)
            return false
          } else {
            tb.reconstructFormattingElements()
            tb.insert(startTag)
          }
        case EndTag =>
          val endTag: Token.EndTag = t.asEndTag
          val name = endTag.name
          if (name == "body") {
            if (!tb.inScope("body")) {
              tb.error(this)
              return false
            } else {
              // todo: error if stack contains something not dd, dt, li, optgroup, option, p, rp, rt, tbody, td, tfoot, th, thead, tr, body, html
              tb.transition(AfterBody)
            }
          } else if (name == "html") {
            val notIgnored: Boolean = tb.processEndTag("body")
            if (notIgnored) {
              return tb.process(endTag)
            }
          } else if (Strings.in(name, Constants.InBodyEndClosers: _*)) {
            if (!tb.inScope(name)) {
              // nothing to close
              tb.error(this)
              return false
            } else {
              tb.generateImpliedEndTags()
              if (!(tb.currentElement.nodeName == name)) {
                tb.error(this)
              }
              tb.popStackToClose(name)
            }
          } else if (name == "form") {
            val currentForm: Element = tb.getFormElement
            tb.setFormElement(null)
            if (currentForm == null || !tb.inScope(name)) {
              tb.error(this)
              return false
            } else {
              tb.generateImpliedEndTags()
              if (!(tb.currentElement.nodeName == name)) {
                tb.error(this)
              }
              // remove currentForm from stack. will shift anything under up.
              tb.removeFromStack(currentForm)
            }
          } else if (name == "p") {
            if (!tb.inButtonScope(name)) {
              tb.error(this)
              tb.processStartTag(name) // if no p to close, creates an empty <p></p>
              return tb.process(endTag)
            } else {
              tb.generateImpliedEndTags(name)
              if (!(tb.currentElement.nodeName == name)) {
                tb.error(this)
              }
              tb.popStackToClose(name)
            }
          } else if (name == "li") {
            if (!tb.inListItemScope(name)) {
              tb.error(this)
              return false
            } else {
              tb.generateImpliedEndTags(name)
              if (!(tb.currentElement.nodeName == name)) {
                tb.error(this)
              }
              tb.popStackToClose(name)
            }
          } else if (Strings.in(name, Constants.DdDt: _*)) {
            if (!tb.inScope(name)) {
              tb.error(this)
              return false
            } else {
              tb.generateImpliedEndTags(name)
              if (!(tb.currentElement.nodeName == name)) {
                tb.error(this)
              }
              tb.popStackToClose(name)
            }
          } else if (Strings.in(name, Constants.Headings: _*)) {
            if (!tb.inScope(Constants.Headings)) {
              tb.error(this)
              return false
            } else {
              tb.generateImpliedEndTags(name)
              if (!(tb.currentElement.nodeName == name)) {
                tb.error(this)
              }
              tb.popStackToClose(Constants.Headings: _*)
            }
          } else if (name == "sarcasm") {
            // *sigh*
            return anyOtherEndTag(t, tb)
          } else if (Strings.in(name, Constants.InBodyEndAdoptionFormatters: _*)) {
            // Adoption Agency Algorithm.
            for (i <- 0 to 7) {
              val formatEl: Element = tb.getActiveFormattingElement(name)
              if (formatEl == null) {
                return anyOtherEndTag(t, tb)
              }
              else if (!tb.onStack(formatEl)) {
                tb.error(this)
                tb.removeFromActiveFormattingElements(formatEl)
                return true
              } else if (!tb.inScope(formatEl.nodeName())) {
                tb.error(this)
                return false
              } else if (tb.currentElement ne formatEl) {
                tb.error(this)
              }

              var furthestBlock: Element = null
              var commonAncestor: Element = null
              var seenFormattingElement: Boolean = false
              val stack: mutable.Buffer[Element] = tb.getStack
              // the spec doesn't limit to < 64, but in degenerate cases (9000+ stack depth) this prevents
              // run-aways
              val stackSize: Int = stack.size
              val min: Int = if (stackSize < 64) {
                stackSize
              } else {
                64
              }
              val inner1 = new Breaks
              inner1.breakable {
                for (si <- 0 to (min - 1)) {
                  val el: Element = stack(si)
                  if (el eq formatEl) {
                    commonAncestor = stack(si - 1)
                    seenFormattingElement = true
                  } else if (seenFormattingElement && tb.isSpecial(el)) {
                    furthestBlock = el
                    inner1.break()
                  }
                }
              }

              if (furthestBlock == null) {
                tb.popStackToClose(formatEl.nodeName())
                tb.removeFromActiveFormattingElements(formatEl)
                return true
              }

              // todo: Let a bookmark note the position of the formatting element in the list of active formatting elements relative to the elements on either side of it in the list.
              // does that mean: int pos of format el in list?
              var node: Element = furthestBlock
              var lastNode: Element = furthestBlock
              val inner2 = new Breaks
              inner2.breakable {
                for (j <- 0 to 2) {
                  val continue = new Breaks
                  continue.breakable {
                    if (tb.onStack(node)) {
                      node = tb.aboveOnStack(node)
                    }
                    if (!tb.isInActiveFormattingElements(node)) {
                      tb.removeFromStack(node)
                      continue.break()
                    } else if (node eq formatEl) {
                      inner2.break()
                    }
                    val replacement: Element = new Element(Tag(node.nodeName()), tb.getBaseUri)
                    tb.replaceActiveFormattingElement(node, replacement)
                    tb.replaceOnStack(node, replacement)
                    node = replacement

                    if (lastNode eq furthestBlock) {
                      // todo: move the aforementioned bookmark to be immediately after the new node in the list of active formatting elements.
                      // not getting how this bookmark both straddles the element above, but is inbetween here...
                    }
                    if (lastNode.parent != null) {
                      lastNode.remove()
                    }

                    node.appendChild(lastNode)
                    lastNode = node
                  }
                }
              }

              if (Strings.in(commonAncestor.nodeName(), Constants.InBodyEndTableFosters: _*)) {
                if (lastNode.parent != null) {
                  lastNode.remove()
                }
                tb.insertInFosterParent(lastNode)
              } else {
                if (lastNode.parent != null) {
                  lastNode.remove()
                }
                commonAncestor.appendChild(lastNode)
              }

              val adopter: Element = new Element(formatEl.tag, tb.getBaseUri)
              adopter.attributes.addAll(formatEl.attributes)
              val childNodes: Array[Node] = furthestBlock.getChildNodes.toArray
              for (childNode <- childNodes) {
                adopter.appendChild(childNode) // append will reparent. thus the clone to avoid concurrent mod.
              }
              furthestBlock.appendChild(adopter)
              tb.removeFromActiveFormattingElements(formatEl)
              // todo: insert the new element into the list of active formatting elements at the position of the aforementioned bookmark.
              tb.removeFromStack(formatEl)
              tb.insertOnStackAfter(furthestBlock, adopter)
            }
          } else if (Strings.in(name, Constants.InBodyStartApplets: _*)) {
            if (!tb.inScope("name")) {
              if (!tb.inScope(name)) {
                tb.error(this)
                return false
              }
              tb.generateImpliedEndTags()
              if (!(tb.currentElement.nodeName == name)) {
                tb.error(this)
              }
              tb.popStackToClose(name)
              tb.clearFormattingElementsToLastMarker()
            }
          } else if (name == "br") {
            tb.error(this)
            tb.processStartTag("br")
            return false
          } else {
            return anyOtherEndTag(t, tb)
          }
        case EOF =>
        // todo: error if stack contains something not dd, dt, li, p, tbody, td, tfoot, th, thead, tr, body, html
        // stop parsing
      }
      true
    }

    private[parser] def anyOtherEndTag(t: Token, tb: HtmlTreeBuilder): Boolean = {
      val name: String = t.asEndTag.name
      val stack: mutable.Buffer[Element] = tb.getStack
      breakable {
        for (pos <- (stack.size - 1).to(0, -1)) {
          val node: Element = stack(pos)
          if (node.nodeName == name) {
            tb.generateImpliedEndTags(name)
            if (!(name == tb.currentElement.nodeName)) {
              tb.error(this)
            }
            tb.popStackToClose(name)
            break()
          } else {
            if (tb.isSpecial(node)) {
              tb.error(this)
              return false
            }
          }
        }
      }
      true
    }
  }

  private[parser] case object Text extends BuilderState {

    // in script, style etc. normally treated as data tags
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Boolean = {
      if (t.isCharacter) {
        tb.insert(t.asCharacter)
      } else if (t.isEOF) {
        tb.error(this)
        // if current node is script: already started
        tb.pop
        tb.transition(tb.originalState)
        return tb.process(t)
      } else if (t.isEndTag) {
        // if: An end tag whose tag name is "script" -- scripting nesting level, if evaluating scripts
        tb.pop
        tb.transition(tb.originalState)
      }
      true
    }
  }

  private[parser] case object InTable extends BuilderState {

    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Boolean = {
      if (t.isCharacter) {
        tb.newPendingTableCharacters()
        tb.markInsertionMode()
        tb.transition(InTableText)
        return tb.process(t)
      } else if (t.isComment) {
        tb.insert(t.asComment)
        return true
      } else if (t.isDoctype) {
        tb.error(this)
        return false
      } else if (t.isStartTag) {
        val startTag: Token.StartTag = t.asStartTag
        val name: String = startTag.name
        if (name == "caption") {
          tb.clearStackToTableContext()
          tb.insertMarkerToFormattingElements()
          tb.insert(startTag)
          tb.transition(InCaption)
        } else if (name == "colgroup") {
          tb.clearStackToTableContext()
          tb.insert(startTag)
          tb.transition(InColumnGroup)
        } else if (name == "col") {
          tb.processStartTag("colgroup")
          return tb.process(t)
        } else if (Strings.in(name, "tbody", "tfoot", "thead")) {
          tb.clearStackToTableContext()
          tb.insert(startTag)
          tb.transition(InTableBody)
        } else if (Strings.in(name, "td", "th", "tr")) {
          tb.processStartTag("tbody")
          return tb.process(t)
        } else if (name == "table") {
          tb.error(this)
          val processed: Boolean = tb.processEndTag("table")
          if (processed) {
            // only ignored if in fragment
            return tb.process(t)
          }
        } else if (Strings.in(name, "style", "script")) {
          return tb.process(t, InHead)
        } else if (name == "input") {
          if (!startTag.attributes.get("type").equalsIgnoreCase("hidden")) {
            return anythingElse(t, tb)
          } else {
            tb.insertEmpty(startTag)
          }
        } else if (name == "form") {
          tb.error(this)
          if (tb.getFormElement != null) {
            return false
          }
          else {
            tb.insertForm(startTag, false)
          }
        } else {
          return anythingElse(t, tb)
        }
        return true // todo: check if should return processed http://www.whatwg.org/specs/web-apps/current-work/multipage/tree-construction.html#parsing-main-intable
      } else if (t.isEndTag) {
        val endTag: Token.EndTag = t.asEndTag
        val name: String = endTag.name
        if (name == "table") {
          if (!tb.inTableScope(name)) {
            tb.error(this)
            return false
          } else {
            tb.popStackToClose("table")
          }
          tb.resetInsertionMode()
        } else if (Strings.in(name, "body", "caption", "col", "colgroup", "html", "tbody", "td", "tfoot", "th", "thead", "tr")) {
          tb.error(this)
          return false
        } else {
          return anythingElse(t, tb)
        }
        return true // todo: as above todo
      } else if (t.isEOF) {
        if (tb.currentElement.nodeName == "html") {
          tb.error(this)
        }
        return true // stops parsing
      }
      anythingElse(t, tb)
    }

    private[parser] def anythingElse(t: Token, tb: HtmlTreeBuilder): Boolean = {
      tb.error(this)
      var processed: Boolean = false
      if (Strings.in(tb.currentElement.nodeName(), "table", "tbody", "tfoot", "thead", "tr")) {
        tb.setFosterInserts(true)
        processed = tb.process(t, InBody)
        tb.setFosterInserts(false)
      } else {
        processed = tb.process(t, InBody)
      }
      processed
    }
  }

  private[parser] case object InTableText extends BuilderState {

    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Boolean = {
      t.tokType match {
        case Character =>
          val c: Token.Character = t.asCharacter
          if (c.getData == nullString) {
            tb.error(this)
            return false
          } else {
            tb.getPendingTableCharacters.append(c.getData)
          }
        case _ =>
          // todo - don't really like the way these table character data lists are built
          if (tb.getPendingTableCharacters.size > 0) {
            for (character <- tb.getPendingTableCharacters) {
              if (!isWhitespace(character)) {
                tb.error(this)
                if (Strings.in(tb.currentElement.nodeName(), "table", "tbody", "tfoot", "thead", "tr")) {
                  tb.setFosterInserts(true)
                  tb.process(new Token.Character().data(character), InBody)
                  tb.setFosterInserts(false)
                } else {
                  tb.process(new Token.Character().data(character), InBody)
                }
              } else {
                tb.insert(new Token.Character().data(character))
              }
            }
            tb.newPendingTableCharacters()
          }
          tb.transition(tb.originalState)
          return tb.process(t)
      }
      true
    }
  }

  private[parser] case object InCaption extends BuilderState {

    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Boolean = {
      if (t.isEndTag && (t.asEndTag.name == "caption")) {
        val endTag: Token.EndTag = t.asEndTag
        val name: String = endTag.name
        if (!tb.inTableScope(name)) {
          tb.error(this)
          return false
        } else {
          tb.generateImpliedEndTags()
          if (!(tb.currentElement.nodeName == "caption")) {
            tb.error(this)
          }
          tb.popStackToClose("caption")
          tb.clearFormattingElementsToLastMarker()
          tb.transition(InTable)
        }
      } else if (t.isStartTag && Strings.in(t.asStartTag.name, "caption", "col", "colgroup", "tbody", "td", "tfoot", "th", "thead", "tr") || t.isEndTag && (t.asEndTag.name == "table")) {
        tb.error(this)
        val processed: Boolean = tb.processEndTag("caption")
        if (processed) {
          return tb.process(t)
        }
      } else if (t.isEndTag && Strings.in(t.asEndTag.name, "body", "col", "colgroup", "html", "tbody", "td", "tfoot", "th", "thead", "tr")) {
        tb.error(this)
        return false
      } else {
        return tb.process(t, InBody)
      }
      true
    }
  }

  private[parser] case object InColumnGroup extends BuilderState {

    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Boolean = {
      if (HtmlTreeBuilderState.isWhitespace(t)) {
        tb.insert(t.asCharacter)
        return true
      }
      t.tokType match {
        case Comment =>
          tb.insert(t.asComment)
        case Doctype =>
          tb.error(this)
        case StartTag =>
          val startTag: Token.StartTag = t.asStartTag
          var name: String = startTag.name
          if (name == "html") {
            return tb.process(t, InBody)
          }
          else if (name == "col") {
            tb.insertEmpty(startTag)
          }
          else {
            return anythingElse(t, tb)
          }
        case EndTag =>
          val endTag: Token.EndTag = t.asEndTag
          val name = endTag.name
          if (name == "colgroup") {
            if (tb.currentElement.nodeName == "html") {
              tb.error(this)
              return false
            } else {
              tb.pop
              tb.transition(InTable)
            }
          } else {
            return anythingElse(t, tb)
          }
        case EOF =>
          if (tb.currentElement.nodeName == "html") {
            return true
          } // stop parsing; frag case
          else {
            return anythingElse(t, tb)
          }
        case _ =>
          return anythingElse(t, tb)
      }
      true
    }

    private def anythingElse(t: Token, tb: TreeBuilder): Boolean = {
      val processed: Boolean = tb.processEndTag("colgroup")
      if (processed) {
        return tb.process(t)
      } // only ignored in frag case
      true
    }
  }

  private[parser] case object InTableBody extends BuilderState {

    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Boolean = {
      t.tokType match {
        case StartTag =>
          val startTag: Token.StartTag = t.asStartTag
          var name: String = startTag.name
          if (name == "tr") {
            tb.clearStackToTableBodyContext()
            tb.insert(startTag)
            tb.transition(InRow)
          } else if (Strings.in(name, "th", "td")) {
            tb.error(this)
            tb.processStartTag("tr")
            return tb.process(startTag)
          } else if (Strings.in(name, "caption", "col", "colgroup", "tbody", "tfoot", "thead")) {
            return exitTableBody(t, tb)
          } else {
            return anythingElse(t, tb)
          }
        case EndTag =>
          val endTag: Token.EndTag = t.asEndTag
          val name = endTag.name
          if (Strings.in(name, "tbody", "tfoot", "thead")) {
            if (!tb.inTableScope(name)) {
              tb.error(this)
              return false
            } else {
              tb.clearStackToTableBodyContext()
              tb.pop
              tb.transition(InTable)
            }
          } else if (name == "table") {
            return exitTableBody(t, tb)
          } else if (Strings.in(name, "body", "caption", "col", "colgroup", "html", "td", "th", "tr")) {
            tb.error(this)
            return false
          } else {
            return anythingElse(t, tb)
          }
        case _ =>
          return anythingElse(t, tb)
      }
      true
    }

    private def exitTableBody(t: Token, tb: HtmlTreeBuilder): Boolean = {
      if (!(tb.inTableScope("tbody") || tb.inTableScope("thead") || tb.inScope("tfoot"))) {
        tb.error(this)
        return false
      }
      tb.clearStackToTableBodyContext()
      tb.processEndTag(tb.currentElement.nodeName())
      tb.process(t)
    }

    private def anythingElse(t: Token, tb: HtmlTreeBuilder): Boolean = {
      tb.process(t, InTable)
    }
  }

  private[parser] case object InRow extends BuilderState {

    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Boolean = {
      if (t.isStartTag) {
        val startTag: Token.StartTag = t.asStartTag
        val name: String = startTag.name
        if (Strings.in(name, "th", "td")) {
          tb.clearStackToTableRowContext()
          tb.insert(startTag)
          tb.transition(InCell)
          tb.insertMarkerToFormattingElements()
        } else if (Strings.in(name, "caption", "col", "colgroup", "tbody", "tfoot", "thead", "tr")) {
          return handleMissingTr(t, tb)
        } else {
          return anythingElse(t, tb)
        }
      } else if (t.isEndTag) {
        val endTag: Token.EndTag = t.asEndTag
        val name: String = endTag.name
        if (name == "tr") {
          if (!tb.inTableScope(name)) {
            tb.error(this)
            return false
          }
          tb.clearStackToTableRowContext()
          tb.pop
          tb.transition(InTableBody)
        } else if (name == "table") {
          return handleMissingTr(t, tb)
        } else if (Strings.in(name, "tbody", "tfoot", "thead")) {
          if (!tb.inTableScope(name)) {
            tb.error(this)
            return false
          }
          tb.processEndTag("tr")
          return tb.process(t)
        } else if (Strings.in(name, "body", "caption", "col", "colgroup", "html", "td", "th")) {
          tb.error(this)
          return false
        } else {
          return anythingElse(t, tb)
        }
      } else {
        return anythingElse(t, tb)
      }
      true
    }

    private def anythingElse(t: Token, tb: HtmlTreeBuilder): Boolean = {
      tb.process(t, InTable)
    }

    private def handleMissingTr(t: Token, tb: TreeBuilder): Boolean = {
      val processed: Boolean = tb.processEndTag("tr")
      if (processed) {
        tb.process(t)
      }
      else {
        false
      }
    }
  }

  private[parser] case object InCell extends BuilderState {

    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Boolean = {
      if (t.isEndTag) {
        val endTag: Token.EndTag = t.asEndTag
        val name: String = endTag.name
        if (Strings.in(name, "td", "th")) {
          if (!tb.inTableScope(name)) {
            tb.error(this)
            tb.transition(InRow)
            return false
          }
          tb.generateImpliedEndTags()
          if (!(tb.currentElement.nodeName == name)) {
            tb.error(this)
          }
          tb.popStackToClose(name)
          tb.clearFormattingElementsToLastMarker()
          tb.transition(InRow)
        } else if (Strings.in(name, "body", "caption", "col", "colgroup", "html")) {
          tb.error(this)
          return false
        } else if (Strings.in(name, "table", "tbody", "tfoot", "thead", "tr")) {
          if (!tb.inTableScope(name)) {
            tb.error(this)
            return false
          }
          closeCell(tb)
          return tb.process(t)
        } else {
          return anythingElse(t, tb)
        }
      } else if (t.isStartTag && Strings.in(t.asStartTag.name, "caption", "col", "colgroup", "tbody", "td", "tfoot", "th", "thead", "tr")) {
        if (!(tb.inTableScope("td") || tb.inTableScope("th"))) {
          tb.error(this)
          return false
        }
        closeCell(tb)
        return tb.process(t)
      } else {
        return anythingElse(t, tb)
      }
      true
    }

    private def anythingElse(t: Token, tb: HtmlTreeBuilder): Boolean = {
      tb.process(t, InBody)
    }

    private def closeCell(tb: HtmlTreeBuilder) {
      if (tb.inTableScope("td")) {
        tb.processEndTag("td")
      }
      else {
        tb.processEndTag("th")
      }
    }
  }

  private[parser] case object InSelect extends BuilderState {

    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Boolean = {
      t.tokType match {
        case Character =>
          val c: Token.Character = t.asCharacter
          if (c.getData == nullString) {
            tb.error(this)
            return false
          } else {
            tb.insert(c)
          }
        case Comment =>
          tb.insert(t.asComment)
        case Doctype =>
          tb.error(this)
          return false
        case StartTag =>
          val start: Token.StartTag = t.asStartTag
          var name: String = start.name
          if (name == "html") {
            return tb.process(start, InBody)
          }
          else if (name == "option") {
            tb.processEndTag("option")
            tb.insert(start)
          } else if (name == "optgroup") {
            if (tb.currentElement.nodeName == "option") {
              tb.processEndTag("option")
            }
            else if (tb.currentElement.nodeName == "optgroup") {
              tb.processEndTag("optgroup")
            }
            tb.insert(start)
          } else if (name == "select") {
            tb.error(this)
            return tb.processEndTag("select")
          } else if (Strings.in(name, "input", "keygen", "textarea")) {
            tb.error(this)
            if (!tb.inSelectScope("select")) {
              return false
            }
            tb.processEndTag("select")
            return tb.process(start)
          } else if (name == "script") {
            return tb.process(t, InHead)
          } else {
            return anythingElse(t, tb)
          }
        case EndTag =>
          val end: Token.EndTag = t.asEndTag
          val name = end.name
          if (name == "optgroup") {
            if ((tb.currentElement.nodeName == "option") && tb.aboveOnStack(tb.currentElement) != null && (tb.aboveOnStack(tb.currentElement).nodeName == "optgroup")) {
              tb.processEndTag("option")
            }
            if (tb.currentElement.nodeName == "optgroup") {
              tb.pop
            }
            else {
              tb.error(this)
            }
          } else if (name == "option") {
            if (tb.currentElement.nodeName == "option") {
              tb.pop
            }
            else {
              tb.error(this)
            }
          } else if (name == "select") {
            if (!tb.inSelectScope(name)) {
              tb.error(this)
              return false
            } else {
              tb.popStackToClose(name)
              tb.resetInsertionMode()
            }
          } else {
            return anythingElse(t, tb)
          }
        case EOF =>
          if (!(tb.currentElement.nodeName == "html")) {
            tb.error(this)
          }
        case _ =>
          return anythingElse(t, tb)
      }
      true
    }

    private def anythingElse(t: Token, tb: HtmlTreeBuilder): Boolean = {
      tb.error(this)
      false
    }
  }

  private[parser] case object InSelectInTable extends BuilderState {

    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Boolean = {
      if (t.isStartTag && Strings.in(t.asStartTag.name, "caption", "table", "tbody", "tfoot", "thead", "tr", "td", "th")) {
        tb.error(this)
        tb.processEndTag("select")
        tb.process(t)
      } else if (t.isEndTag && Strings.in(t.asEndTag.name, "caption", "table", "tbody", "tfoot", "thead", "tr", "td", "th")) {
        tb.error(this)
        if (tb.inTableScope(t.asEndTag.name)) {
          tb.processEndTag("select")
          tb.process(t)
        } else {
          false
        }
      } else {
        tb.process(t, InSelect)
      }
    }
  }

  private[parser] case object AfterBody extends BuilderState {

    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Boolean = {
      if (isWhitespace(t)) {
        return tb.process(t, InBody)
      } else if (t.isComment) {
        tb.insert(t.asComment) // into html node
      } else if (t.isDoctype) {
        tb.error(this)
        return false
      } else if (t.isStartTag && (t.asStartTag.name == "html")) {
        return tb.process(t, InBody)
      } else if (t.isEndTag && (t.asEndTag.name == "html")) {
        if (tb.isFragmentParsing) {
          tb.error(this)
          return false
        } else {
          tb.transition(AfterAfterBody)
        }
      } else if (t.isEOF) {
        // chillax! we're done
      } else {
        tb.error(this)
        tb.transition(InBody)
        return tb.process(t)
      }
      true
    }
  }

  private[parser] case object InFrameset extends BuilderState {

    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Boolean = {
      if (isWhitespace(t)) {
        tb.insert(t.asCharacter)
      } else if (t.isComment) {
        tb.insert(t.asComment)
      } else if (t.isDoctype) {
        tb.error(this)
        return false
      } else if (t.isStartTag) {
        val start: Token.StartTag = t.asStartTag
        val name: String = start.name
        if (name == "html") {
          return tb.process(start, InBody)
        } else if (name == "frameset") {
          tb.insert(start)
        } else if (name == "frame") {
          tb.insertEmpty(start)
        } else if (name == "noframes") {
          return tb.process(start, InHead)
        } else {
          tb.error(this)
          return false
        }
      } else if (t.isEndTag && (t.asEndTag.name == "frameset")) {
        if (tb.currentElement.nodeName == "html") {
          // frag
          tb.error(this)
          return false
        } else {
          tb.pop
          if (!tb.isFragmentParsing && !(tb.currentElement.nodeName == "frameset")) {
            tb.transition(AfterFrameset)
          }
        }
      } else if (t.isEOF) {
        if (!(tb.currentElement.nodeName == "html")) {
          tb.error(this)
          return true
        }
      } else {
        tb.error(this)
        return false
      }
      true
    }
  }

  private[parser] case object AfterFrameset extends BuilderState {

    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Boolean = {
      if (isWhitespace(t)) {
        tb.insert(t.asCharacter)
      } else if (t.isComment) {
        tb.insert(t.asComment)
      } else if (t.isDoctype) {
        tb.error(this)
        return false
      } else if (t.isStartTag && (t.asStartTag.name == "html")) {
        return tb.process(t, InBody)
      } else if (t.isEndTag && (t.asEndTag.name == "html")) {
        tb.transition(AfterAfterFrameset)
      } else if (t.isStartTag && (t.asStartTag.name == "noframes")) {
        return tb.process(t, InHead)
      } else if (t.isEOF) {
        // cool your heels, we're complete
      } else {
        tb.error(this)
        return false
      }
      true
    }
  }

  private[parser] case object AfterAfterBody extends BuilderState {

    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Boolean = {
      if (t.isComment) {
        tb.insert(t.asComment)
      } else if (t.isDoctype || isWhitespace(t) || (t.isStartTag && (t.asStartTag.name == "html"))) {
        return tb.process(t, InBody)
      } else if (t.isEOF) {
        // nice work chuck
      } else {
        tb.error(this)
        tb.transition(InBody)
        return tb.process(t)
      }
      true
    }
  }

  private[parser] case object AfterAfterFrameset extends BuilderState {

    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Boolean = {
      if (t.isComment) {
        tb.insert(t.asComment)
      } else if (t.isDoctype || isWhitespace(t) || (t.isStartTag && (t.asStartTag.name == "html"))) {
        return tb.process(t, InBody)
      } else if (t.isEOF) {
        // nice work chuck
      } else if (t.isStartTag && (t.asStartTag.name == "noframes")) {
        return tb.process(t, InHead)
      } else {
        tb.error(this)
        return false
      }
      true
    }
  }

  private[parser] case object ForeignContent extends BuilderState {

    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Boolean = {
      true
      // todo: implement. Also; how do we get here?
    }
  }

  private[HtmlTreeBuilderState] var nullString: String = String.valueOf('\u0000')

  private[HtmlTreeBuilderState] def isWhitespace(t: Token): Boolean = {
    if (t.isCharacter) {
      val data: String = t.asCharacter.getData
      return isWhitespace(data)
    }
    false
  }

  private[HtmlTreeBuilderState] def isWhitespace(data: String): Boolean = {
    // todo: this checks more than spec - "\t", "\n", "\f", "\r", " "
    for (i <- 0 to (data.length() - 1) if !Strings.isWhitespace(data.charAt(i))) {
      return false
    }
    true
  }

  private[HtmlTreeBuilderState] def handleRcData(startTag: Token.StartTag, tb: HtmlTreeBuilder) {
    tb.insert(startTag)
    tb.tokeniser.transition(Rcdata)
    tb.markInsertionMode()
    tb.transition(Text)
  }

  private[HtmlTreeBuilderState] def handleRawtext(startTag: Token.StartTag, tb: HtmlTreeBuilder) {
    tb.insert(startTag)
    tb.tokeniser.transition(Rawtext)
    tb.markInsertionMode()
    tb.transition(Text)
  }

  private[HtmlTreeBuilderState] object Constants {

    private[HtmlTreeBuilderState] val InBodyStartToHead: Array[String] = Array[String]("base", "basefont", "bgsound", "command", "link", "meta", "noframes", "script", "style", "title")
    private[HtmlTreeBuilderState] val InBodyStartPClosers: Array[String] = Array[String]("address", "article", "aside", "blockquote", "center", "details", "dir", "div", "dl", "fieldset", "figcaption", "figure", "footer", "header", "hgroup", "menu", "nav", "ol", "p", "section", "summary", "ul")
    private[HtmlTreeBuilderState] val Headings: Array[String] = Array[String]("h1", "h2", "h3", "h4", "h5", "h6")
    private[HtmlTreeBuilderState] val InBodyStartPreListing: Array[String] = Array[String]("pre", "listing")
    private[HtmlTreeBuilderState] val InBodyStartLiBreakers: Array[String] = Array[String]("address", "div", "p")
    private[HtmlTreeBuilderState] val DdDt: Array[String] = Array[String]("dd", "dt")
    private[HtmlTreeBuilderState] val Formatters: Array[String] = Array[String]("b", "big", "code", "em", "font", "i", "s", "small", "strike", "strong", "tt", "u")
    private[HtmlTreeBuilderState] val InBodyStartApplets: Array[String] = Array[String]("applet", "marquee", "object")
    private[HtmlTreeBuilderState] val InBodyStartEmptyFormatters: Array[String] = Array[String]("area", "br", "embed", "img", "keygen", "wbr")
    private[HtmlTreeBuilderState] val InBodyStartMedia: Array[String] = Array[String]("param", "source", "track")
    private[HtmlTreeBuilderState] val InBodyStartInputAttribs: Array[String] = Array[String]("name", "action", "prompt")
    private[HtmlTreeBuilderState] val InBodyStartOptions: Array[String] = Array[String]("optgroup", "option")
    private[HtmlTreeBuilderState] val InBodyStartRuby: Array[String] = Array[String]("rp", "rt")
    private[HtmlTreeBuilderState] val InBodyStartDrop: Array[String] = Array[String]("caption", "col", "colgroup", "frame", "head", "tbody", "td", "tfoot", "th", "thead", "tr")
    private[HtmlTreeBuilderState] val InBodyEndClosers: Array[String] = Array[String]("address", "article", "aside", "blockquote", "button", "center", "details", "dir", "div", "dl", "fieldset", "figcaption", "figure", "footer", "header", "hgroup", "listing", "menu", "nav", "ol", "pre", "section", "summary", "ul")
    private[HtmlTreeBuilderState] val InBodyEndAdoptionFormatters: Array[String] = Array[String]("a", "b", "big", "code", "em", "font", "i", "nobr", "s", "small", "strike", "strong", "tt", "u")
    private[HtmlTreeBuilderState] val InBodyEndTableFosters: Array[String] = Array[String]("table", "tbody", "tfoot", "thead", "tr")
  }

}