package com.github.ligangty.scala.jsoup.parser

import com.github.ligangty.scala.jsoup.helper.Strings
import com.github.ligangty.scala.jsoup.nodes.DocumentType

/**
 * The Tree Builder's current state. Each state embodies the processing for the state, and transitions to other states.
 */
private[parser] object HtmlTreeBuilderState {

  private[parser] sealed trait BuilderState {
    private[parser] def process(t: Token, tb: HtmlTreeBuilder)
  }

  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit = {
      if (isWhitespace(t)) {
        return true
      }
      else if (t.isComment) {
        tb.insert(t.asComment)
      }
      else if (t.isDoctype) {
        val d: Token.Doctype = t.asDoctype
        val doctype: DocumentType = new DocumentType(d.getName, d.getPublicIdentifier, d.getSystemIdentifier, tb.getBaseUri)
        tb.getDocument.appendChild(doctype)
        if (d.isForceQuirks) {
          tb.getDocument.quirksMode(Document.QuirksMode.quirks)
        }
        tb.transition(BeforeHtml)
      }
      else {
        tb.transition(BeforeHtml)
        return tb.process(t)
      }
      return true
    }
  }

  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

    }
  }
  private[parser] case object Initial extends BuilderState {
    override private[parser] def process(t: Token, tb: HtmlTreeBuilder): Unit ={

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
    for (i <- 0 to (data.length() - 1)) {
      if (!Strings.isWhitespace(data.charAt(i)))
        return false
    }
    true
  }

  private[HtmlTreeBuilderState] def handleRcData(startTag: Token.StartTag, tb: HtmlTreeBuilder) {
    tb.insert(startTag)
    tb.tokeniser.transition(Rcdata)
    tb.markInsertionMode
    tb.transition(Text)
  }

  private[HtmlTreeBuilderState] def handleRawtext(startTag: Token.StartTag, tb: HtmlTreeBuilder) {
    tb.insert(startTag)
    tb.tokeniser.transition(Rawtext)
    tb.markInsertionMode
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