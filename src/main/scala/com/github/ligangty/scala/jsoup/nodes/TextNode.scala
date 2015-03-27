package com.github.ligangty.scala.jsoup.nodes

import com.github.ligangty.scala.jsoup.helper.Strings
import com.github.ligangty.scala.jsoup.helper.Strings._
import TextNode._
import com.github.ligangty.scala.jsoup.helper.Validator._

/**
 * Created by gli on 3/26/15.
 */
class TextNode extends Node {
  private[nodes] var textVal: String = null

  /**
   * Create a new TextNode representing the supplied (unencoded) text).
   *
   * @param text raw text
   * @param baseUri base uri
   * @see #createFromEncoded(String, String)
   */
  def this(text: String, baseUri: String) {
    this()
    this.baseUriVal = baseUri
    this.textVal = text
  }


  def nodeName: String = {
    return "#text"
  }

  /**
   * Get the text content of this text node.
   * @return Unencoded, normalised text.
   * @see TextNode#getWholeText()
   */
  def text: String = {
    return normaliseWhitespace(getWholeText)
  }

  /**
   * Set the text content of this text node.
   * @param text unencoded text
   * @return this, for chaining
   */
  def text(text: String): TextNode = {
    this.textVal = text
    if (attributes != null) attributes.put(TEXT_KEY, text)
    return this
  }

  /**
  Get the (unencoded) text of this text node, including any newlines and spaces present in the original.
     @return text
    */
  def getWholeText: String = {
    return if (attributes == null) text else attributes.get(TEXT_KEY)
  }

  /**
  Test if this text node is blank -- that is, empty or only whitespace (including newlines).
     @return true if this document is empty or only whitespace, false if it contains any text content.
    */
  def isBlank: Boolean = Strings.isBlank(getWholeText)

  /**
   * Split this text node into two nodes at the specified string offset. After splitting, this node will contain the
   * original text up to the offset, and will have a new text node sibling containing the text after the offset.
   * @param offset string offset point to split node at.
   * @return the newly created text node containing the text after the offset.
   */
  def splitText(offset: Int): TextNode = {
    isTrue(offset >= 0, "Split offset must be not be negative")
    isTrue(offset < text.length, "Split offset must not be greater than current text length")
    val head: String = getWholeText.substring(0, offset)
    val tail: String = getWholeText.substring(offset)
    text(head)
    val tailNode: TextNode = new TextNode(tail, this.baseUri)
    if (parent != null) parent.addChildren(siblingIndex + 1, tailNode)
    tailNode
  }

  private[nodes] def outerHtmlHead(accum: StringBuilder, depth: Int, out: Document.OutputSettings) {
    if (out.prettyPrint && ((siblingIndex == 0 && parentNode.isInstanceOf[Element] && (parentNode.asInstanceOf[Element]).tag.isFormatAsBlock && !isBlank) || (out.outline && siblingNodes.size > 0 && !isBlank))) indent(accum, depth, out)
    val normaliseWhite: Boolean = out.prettyPrint && parent.isInstanceOf[Element] && !Element.preserveWhitespace(parent.asInstanceOf[Element])
    Entities.escape(accum, getWholeText, out, false, normaliseWhite, false)
  }

  private[nodes] def outerHtmlTail(accum: StringBuilder, depth: Int, out: Document.OutputSettings) {
  }

  override def toString: String = outerHtml

  // attribute fiddling. create on first access.
  private def ensureAttributes {
    if (attributesVal == null) {
      attributesVal = new Attributes
      attributesVal.put(TEXT_KEY, text)
    }
  }

  override def attr(attributeKey: String): String = {
    ensureAttributes
    super.attr(attributeKey)
  }

  override def attributes: Attributes = {
    ensureAttributes
    super.attributes
  }

  override def attr(attributeKey: String, attributeValue: String): Node = {
    ensureAttributes
    super.attr(attributeKey, attributeValue)
  }

  override def hasAttr(attributeKey: String): Boolean = {
    ensureAttributes
    super.hasAttr(attributeKey)
  }

  override def removeAttr(attributeKey: String): Node = {
    ensureAttributes
    super.removeAttr(attributeKey)
  }

  override def absUrl(attributeKey: String): String = {
    ensureAttributes
    super.absUrl(attributeKey)
  }
}

object TextNode {
  /*
   TextNode is a node, and so by default comes with attributes and children. The attributes are seldom used, but use
   memory, and the child nodes are never used. So we don't have them, and override accessors to attributes to create
   them as needed on the fly.
    */
  private val TEXT_KEY = "text"

  /**
   * Create a new TextNode from HTML encoded (aka escaped) data.
   * @param encodedText Text containing encoded HTML (e.g. &amp;lt;)
   * @param baseUri Base uri
   * @return TextNode containing unencoded data (e.g. &lt;)
   */
  def createFromEncoded(encodedText: String, baseUri: String): TextNode =
    new TextNode(Entities.unescape(encodedText), baseUri)

  private[nodes] def normaliseWhitespace(text: String): String = Strings.normaliseWhitespace(text)

  private[nodes] def stripLeadingWhitespace(text: String): String = text.replaceFirst("^\\s+", "")

  private[nodes] def lastCharIsWhitespace(sb: java.lang.StringBuilder): Boolean =
    sb.length != 0 && sb.charAt(sb.length - 1) == ' '


}