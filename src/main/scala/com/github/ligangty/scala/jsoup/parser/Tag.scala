package com.github.ligangty.scala.jsoup.parser

import com.github.ligangty.scala.jsoup.helper.Validator._

/**
 * HTML Tag capabilities.
 *
 * @constructor default constructor
 * @param tagName tag name
 * @param block block or inline
 * @param containBlock Can this tag hold block level tags?
 * @param formatAsBlock should be formatted as a block
 *
 */
class Tag private(private var tagName: String, private var block: Boolean, private var containBlock: Boolean, private var formatAsBlock: Boolean) {

  tagName = tagName.toLowerCase
  // only pcdata if not
  private var canContainInline: Boolean = true
  // can hold nothing; e.g. img
  private var empty: Boolean = false
  // can self close (<foo />). used for unknown tags that self close, without forcing them as empty.
  private var selfClosing: Boolean = false
  // for pre, textarea, script etc
  private var preserveWhitespace: Boolean = false
  // a control that appears in forms: input, textarea, output etc
  private var formList: Boolean = false
  // a control that can be submitted in a form: input etc
  private var formSubmit: Boolean = false

  private def this(tagName: String) {
    this(tagName, true, true, true)
  }

  private def this(tagName: String, block: Boolean, containBlock: Boolean) {
    this(tagName, block, containBlock, true)
  }

  /**
   * Get this tag's name.
   *
   * @return the tag's name
   */
  def getName: String = tagName

  /**
   * If this is a block tag.
   */
  def isBlock: Boolean = block

  /**
   * if this tag should be formatted as a block (or as inline)
   */
  def isFormatAsBlock: Boolean = formatAsBlock

  /**
   * if this tag can contain block tags.
   */
  def canContainBlock: Boolean = containBlock

  /**
   * if this is an empty tag
   */
  def isEmpty: Boolean = empty

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
  def isData: Boolean = !canContainInline && !isEmpty

  /**
   * Get if this tag is self closing.
   *
   * @return if this tag should be output as self closing.
   */
  def isSelfClosing: Boolean = {
    isEmpty || selfClosing
  }

  /**
   * Get if this is a pre-defined tag, or was auto created on parsing.
   *
   * @return if a known tag
   */
  def isKnownTag: Boolean = {
    Tag.tags.contains(tagName)
  }

  /**
   * Check if this tagname is a known tag.
   *
   * @param tagName name of tag
   * @return if known HTML tag
   */
  def isKnownTag(tagName: String): Boolean = {
    Tag.tags.contains(tagName)
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
    formList
  }

  /**
   * Get if this tag represents an element that should be submitted with a form. E.g. input, option
   * @return if submittable with a form
   */
  def isFormSubmittable: Boolean = {
    formSubmit
  }

  private[parser] def setSelfClosing(): Tag = {
    selfClosing = true
    this
  }

  override def equals(o: Any): Boolean = o match {
    case that: Tag =>
      that.tagName == this.tagName &&
              that.canContainBlock == this.canContainBlock &&
              that.canContainInline == this.canContainInline &&
              that.isEmpty == this.isEmpty &&
              that.isFormatAsBlock == this.isFormatAsBlock &&
              that.isBlock == this.isBlock &&
              that.preserveWhitespace == this.preserveWhitespace &&
              that.selfClosing == this.selfClosing &&
              that.formList == this.formList &&
              that.formSubmit == this.formSubmit
    case _ => false
  }

  import scala.language.implicitConversions

  implicit private def booleanToInt(boolVal: Boolean): Int = if (boolVal) {
    1
  } else {
    0
  }

  override def hashCode: Int = {
    var result: Int = tagName.hashCode
    result = 31 * result + block
    result = 31 * result + formatAsBlock
    result = 31 * result + containBlock
    result = 31 * result + canContainInline
    result = 31 * result + empty
    result = 31 * result + selfClosing
    result = 31 * result + preserveWhitespace
    result = 31 * result + formList
    result = 31 * result + formSubmit
    result
  }

  override def toString: String = {
    tagName
  }
}

object Tag {

  private val blockTags: Array[String] = Array("html", "head", "body", "frameset", "script", "noscript", "style", "meta", "link", "title", "frame", "noframes", "section", "nav", "aside", "hgroup", "header", "footer", "p", "h1", "h2", "h3", "h4", "h5", "h6", "ul", "ol", "pre", "div", "blockquote", "hr", "address", "figure", "figcaption", "form", "fieldset", "ins", "del", "s", "dl", "dt", "dd", "li", "table", "caption", "thead", "tfoot", "tbody", "colgroup", "col", "tr", "th", "td", "video", "audio", "canvas", "details", "menu", "plaintext", "template", "article", "main", "svg", "math")
  private val inlineTags: Array[String] = Array("object", "base", "font", "tt", "i", "b", "u", "big", "small", "em", "strong", "dfn", "code", "samp", "kbd", "var", "cite", "abbr", "time", "acronym", "mark", "ruby", "rt", "rp", "a", "img", "br", "wbr", "map", "q", "sub", "sup", "bdo", "iframe", "embed", "span", "input", "select", "textarea", "label", "button", "optgroup", "option", "legend", "datalist", "keygen", "output", "progress", "meter", "area", "param", "source", "track", "summary", "command", "device", "area", "basefont", "bgsound", "menuitem", "param", "source", "track", "data", "bdi")
  private val emptyTags: Array[String] = Array("meta", "link", "base", "frame", "img", "br", "wbr", "embed", "hr", "input", "keygen", "col", "command", "device", "area", "basefont", "bgsound", "menuitem", "param", "source", "track")
  private val formatAsInlineTags: Array[String] = Array("title", "a", "p", "h1", "h2", "h3", "h4", "h5", "h6", "pre", "address", "li", "th", "td", "script", "style", "ins", "del", "s")
  private val preserveWhitespaceTags: Array[String] = Array("pre", "plaintext", "title", "textarea")
  private val formListedTags: Array[String] = Array("button", "fieldset", "input", "keygen", "object", "output", "select", "textarea")
  private val formSubmitTags: Array[String] = Array("input", "keygen", "object", "select", "textarea")

  // creates
  private val tags: Map[String, Tag] = initTags()

  /**
   * Get a Tag by name. If not previously defined (unknown), returns a new generic tag, that can do anything.
   * <p>
   * Pre-defined tags (P, DIV etc) will be ==, but unknown tags are not registered and will only .equals().
   * </p>
   *
   * @param tagName Name of tag, e.g. "p". Case insensitive.
   * @return The tag, either defined or new generic.
   */
  def apply(tagName: String): Tag = {
    notNull(tagName)
    tags.get(tagName) match {
      case Some(find1: Tag) => find1
      case None =>
        notEmpty(tagName.trim.toLowerCase)
        tags.get(tagName.trim.toLowerCase) match {
          case Some(find2: Tag) => find2
          case None => new Tag(tagName, false, true)
        }
    }
  }

  private def initTags(): Map[String, Tag] = {
    import scala.collection.mutable
    val tagsV: mutable.Map[String, Tag] = new mutable.HashMap()
    for (tagName <- blockTags) {
      tagsV += (tagName -> new Tag(tagName))
    }
    for (tagName <- inlineTags) {
      tagsV += (tagName -> new Tag(tagName, false, false, false))
    }

    // mods:
    for (tagName <- emptyTags) {
      val tag = tagsV(tagName)
      notNull(tag)
      tag.containBlock = false
      tag.canContainInline = false
      tag.empty = true
      tagsV(tagName) = tag
    }

    for (tagName <- formatAsInlineTags) {
      val tag = tagsV(tagName)
      notNull(tag)
      tag.formatAsBlock = false
      tagsV(tagName) = tag
    }

    for (tagName <- preserveWhitespaceTags) {
      val tag = tagsV(tagName)
      notNull(tag)
      tag.preserveWhitespace = true
      tagsV(tagName) = tag
    }

    for (tagName <- formListedTags) {
      val tag = tagsV(tagName)
      notNull(tag)
      tag.formList = true
      tagsV(tagName) = tag
    }

    for (tagName <- formSubmitTags) {
      val tag = tagsV(tagName)
      notNull(tag)
      tag.formSubmit = true
      tagsV(tagName) = tag
    }
    tagsV.toMap
  }
}