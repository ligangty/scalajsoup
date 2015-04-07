package com.github.ligangty.scala.jsoup.parser

import com.github.ligangty.scala.jsoup.nodes._

import scala.collection.mutable

/**
 * HTML Tree Builder; creates a DOM from Tokens.
 */
private[parser] class HtmlTreeBuilder extends TreeBuilder {
  private var state: HtmlTreeBuilderState.BuilderState = null
  private var originalState: HtmlTreeBuilderState.BuilderState = null
  private var baseUriSetFromDoc: Boolean = false
  private var headElement: Element = null
  private var formElement: FormElement = null
  private var contextElement: Element = null
  private var formattingElements: mutable.ArrayBuffer[Element] = new mutable.ArrayBuffer[Element]
  private var pendingTableCharacters: mutable.Buffer[String] = new mutable.ArrayBuffer[String]
  private var emptyEnd: Token.EndTag = new Token.EndTag
  private var framesetOk: Boolean = true
  private var fosterInserts: Boolean = false
  private var fragmentParsing: Boolean = false

  private[parser] def transition(state: HtmlTreeBuilderState.BuilderState) {
    this.state = state
  }

  private[parser] def markInsertionMode():Unit =  {
    originalState = state
  }

  private[parser] def insert(startTag: Token.StartTag): Element = {
    //todo need to implement
//    if (startTag.isSelfClosing) {
//      val el: Element = insertEmpty(startTag)
//      stack.append(el)
//      tokeniser.transition(Data)
//      tokeniser.emit(emptyEnd.reset.name(el.tagName))
//      return el
//    }
//    val el: Element = new Element(Tag(startTag.name), baseUri, startTag.attributes)
//    insert(el)
//    el
    null
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