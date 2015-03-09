package com.github.ligangty.scala.jsoup.parser

import com.github.ligangty.scala.jsoup.helper.Validator._

/**
 * Created by gli on 3/9/15.
 */
class Tag {

  private[Tag] var tagName: String = null
  /**
   * If this is a block tag.
   */
  private[Tag] var isBlock: Boolean = true
  /**
   * if this tag should be formatted as a block (or as inline)
   */
  private[Tag] var isFormatAsBlock: Boolean = true
  /**
   * if this tag can contain block tags.
   */
  private[Tag] var canContainBlock: Boolean = true
  private[Tag] var canContainInline: Boolean = true
  /**
   * if this is an empty tag
   */
  private[Tag] var isEmpty: Boolean = false
  private[Tag] var selfClosing: Boolean = false
  private[Tag] var preserveWhitespace: Boolean = false
  private[Tag] var formList: Boolean = false
  private[Tag] var formSubmit: Boolean = false

  private def this(tagName: String) {
    this()
    this.tagName = tagName.toLowerCase
  }

  /**
   * Get this tag's name.
   *
   * @return the tag's name
   */
  def getName: String = tagName


  /**
   * Gets if this tag is an inline tag.
   *
   * @return if this tag is an inline tag.
   */
  def isInline: Boolean = !isBlock

  /**
   * Gets if this tag is a data only tag.
   *
   * @return if this tag is a data only tag
   */
  def isData: Boolean = {
    return !canContainInline && !isEmpty
  }


  /**
   * Get if this tag is self closing.
   *
   * @return if this tag should be output as self closing.
   */
  def isSelfClosing: Boolean = {
    return isEmpty || selfClosing
  }

  /**
   * Get if this is a pre-defined tag, or was auto created on parsing.
   *
   * @return if a known tag
   */
  def isKnownTag: Boolean = {
    return Tag.tags.contains(tagName)
  }

  /**
   * Check if this tagname is a known tag.
   *
   * @param tagName name of tag
   * @return if known HTML tag
   */
  def isKnownTag(tagName: String): Boolean = {
    return Tag.tags.contains(tagName)
  }

  /**
   * Get if this tag should preserve whitespace within child text nodes.
   *
   * @return if preserve whitepace
   */
  def isPreserveWhitespace: Boolean = preserveWhitespace

  /**
   * Get if this tag represents a control associated with a form. E.g. input, textarea, output
   * @return if associated with a form
   */
  def isFormListed: Boolean = {
    return formList
  }

  /**
   * Get if this tag represents an element that should be submitted with a form. E.g. input, option
   * @return if submittable with a form
   */
  def isFormSubmittable: Boolean = {
    return formSubmit
  }

  private[parser] def setSelfClosing: Tag = {
    selfClosing = true
    return this
  }

  def equals(o: AnyRef) = o match {
    case that: Tag => {
      that.tagName == this.tagName &&
        that.canContainBlock == this.canContainBlock &&
        that.canContainInline == this.canContainInline &&
        that.isEmpty == this.isEmpty &&
        that.isFormatAsBlock == this.isFormatAsBlock &&
        that.isBlock == this.isBlock &&
        that.preserveWhitespace == this.preserveWhitespace &&
        that.selfClosing = this.selfClosing &&
        that.formList = this.formList &&
        that.formSubmit == this.formSubmit
    }
    case _ => false
  }

  def ==(o: AnyRef) = equals(o)

  override def hashCode: Int = {
    var result: Int = tagName.hashCode
    result = 31 * result + (if (isBlock) 1 else 0)
    result = 31 * result + (if (isFormatAsBlock) 1 else 0)
    result = 31 * result + (if (canContainBlock) 1 else 0)
    result = 31 * result + (if (canContainInline) 1 else 0)
    result = 31 * result + (if (isEmpty) 1 else 0)
    result = 31 * result + (if (selfClosing) 1 else 0)
    result = 31 * result + (if (preserveWhitespace) 1 else 0)
    result = 31 * result + (if (formList) 1 else 0)
    result = 31 * result + (if (formSubmit) 1 else 0)
    return result
  }

  override def toString: String = {
    return tagName
  }


}

object Tag {
  // creates
  private[Tag] val tags: scala.collection.mutable.Map[String, Tag] = scala.collection.mutable.HashMap[String, Tag]()

  private[this] val blockTags: Array[String] = Array("html", "head", "body", "frameset", "script", "noscript", "style", "meta", "link", "title", "frame", "noframes", "section", "nav", "aside", "hgroup", "header", "footer", "p", "h1", "h2", "h3", "h4", "h5", "h6", "ul", "ol", "pre", "div", "blockquote", "hr", "address", "figure", "figcaption", "form", "fieldset", "ins", "del", "s", "dl", "dt", "dd", "li", "table", "caption", "thead", "tfoot", "tbody", "colgroup", "col", "tr", "th", "td", "video", "audio", "canvas", "details", "menu", "plaintext", "template", "article", "main", "svg", "math")
  private[this] val inlineTags: Array[String] = Array("object", "base", "font", "tt", "i", "b", "u", "big", "small", "em", "strong", "dfn", "code", "samp", "kbd", "var", "cite", "abbr", "time", "acronym", "mark", "ruby", "rt", "rp", "a", "img", "br", "wbr", "map", "q", "sub", "sup", "bdo", "iframe", "embed", "span", "input", "select", "textarea", "label", "button", "optgroup", "option", "legend", "datalist", "keygen", "output", "progress", "meter", "area", "param", "source", "track", "summary", "command", "device", "area", "basefont", "bgsound", "menuitem", "param", "source", "track", "data", "bdi")
  private[this] val emptyTags: Array[String] = Array("meta", "link", "base", "frame", "img", "br", "wbr", "embed", "hr", "input", "keygen", "col", "command", "device", "area", "basefont", "bgsound", "menuitem", "param", "source", "track")
  private[this] val formatAsInlineTags: Array[String] = Array("title", "a", "p", "h1", "h2", "h3", "h4", "h5", "h6", "pre", "address", "li", "th", "td", "script", "style", "ins", "del", "s")
  private[this] val preserveWhitespaceTags: Array[String] = Array("pre", "plaintext", "title", "textarea")
  private[this] val formListedTags: Array[String] = Array("button", "fieldset", "input", "keygen", "object", "output", "select", "textarea")
  private[this] val formSubmitTags: Array[String] = Array("input", "keygen", "object", "select", "textarea")

  def apply(tagName: String): Tag = {
    if (tags.isEmpty) {
      init
    }
    notNull(tagName)
    var tag: Tag = tags(tagName)
    if (tag == null) {
      notEmpty(tagName.trim.toLowerCase)
      tag = tags(tagName.trim.toLowerCase)
      if (tag == null) {
        tag = new Tag(tagName)
        tag.isBlock = false
        tag.canContainBlock = true
      }
    }
    tag
  }

  private[this] def init(): Unit = {
    for (tagName <- blockTags) {
      register(new Tag(tagName))
    }
    for (tagName <- inlineTags) {
      val tag = new Tag(tagName)
      tag.isBlock = false
      tag.canContainBlock = false
      tag.isFormatAsBlock = false
      register(tag)
    }

    // mods:
    for (tagName <- emptyTags) {
      val tag = tags(tagName)
      notNull(tag)
      tag.canContainBlock = false
      tag.canContainInline = false
      tag.isEmpty = true
    }

    for (tagName <- formatAsInlineTags) {
      val tag = tags(tagName)
      notNull(tag)
      tag.isFormatAsBlock = false
    }

    for (tagName <- preserveWhitespaceTags) {
      val tag = tags(tagName)
      notNull(tag)
      tag.preserveWhitespace = true
    }

    for (tagName <- formListedTags) {
      val tag = tags(tagName)
      notNull(tag)
      tag.formList = true
    }

    for (tagName <- formSubmitTags) {
      val tag = tags(tagName)
      notNull(tag)
      tag.formSubmit = true
    }
  }

  private def register(tag: Tag) {
    tags(tag.tagName) = tag
  }
}