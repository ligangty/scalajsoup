package com.github.ligangty.scala.jsoup.nodes

import java.nio.charset.{CharsetEncoder, Charset}

import com.github.ligangty.scala.jsoup.helper.Strings
import com.github.ligangty.scala.jsoup.helper.Validator._
import com.github.ligangty.scala.jsoup.nodes.Document.OutputSettings
import com.github.ligangty.scala.jsoup.parser.Tag
import com.github.ligangty.scala.jsoup.select.Elements

import scala.collection.mutable

/**
 * A HTML Document.
 */
class Document private(baseUri: String, locationVal: String) extends Element(Tag("#root"), baseUri) {

  private var outputSettingsVal: Document.OutputSettings = new Document.OutputSettings
  private var quirksModeVal: Document.QuirksMode = Document.QuirksMode.noQuirks
  private var updateMetaCharsetVal: Boolean = false

  def this(baseUri: String) {
    this(baseUri, baseUri)
  }

  /**
   * Get the URL this Document was parsed from. If the starting URL is a redirect,
   * this will return the final URL from which the document was served from.
   * @return location
   */
  def location: String = locationVal

  /**
   * Accessor to the document's <code>head</code> element.
   * @return { @code head}
   */
  def head: Element = findFirstElementByTagName("head", this) match {
    case Some(e: Element) => e
    case None => null
  }

  /**
   * Accessor to the document's <code>body</code> element.
   * @return { @code body}
   */
  def body: Element = findFirstElementByTagName("body", this) match {
    case Some(e: Element) => e
    case None => null
  }

  /**
   * Get the string contents of the document's <code>title</code> element.
   * @return Trimmed title, or empty string if none set.
   */
  def title: String = {
    val titleEl: Element = getElementsByTag("title").first()
    if (titleEl != null) {
      Strings.normaliseWhitespace(titleEl.text()).trim
    } else {
      ""
    }
  }

  /**
   * Set the document's <code>title</code> element. Updates the existing element, or adds <code>title</code> to <code>head</code> if
   * not present
   * @param title string to set as title
   */
  def title(title: String) {
    notNull(title)
    val titleEl: Element = getElementsByTag("title").first()
    if (titleEl == null) {
      head.appendElement("title").text(title)
    } else {
      titleEl.text(title)
    }
  }

  /**
   * Create a new Element, with this document's base uri. Does not make the new element a child of this document.
   * @param tagName element tag name (e.g. <code>a</code)
   * @return new element
   */
  def createElement(tagName: String): Element = {
    new Element(Tag(tagName), this.baseUri)
  }

  /**
   * Normalise the document. This happens after the parse phase so generally does not need to be called.
   * Moves any text content that is not in the body element into the body.
   * @return this document after normalisation
   */
  def normalise: Document = {
    val htmlEl: Element = findFirstElementByTagName("html", this) match {
      case Some(e: Element) => e
      case None => appendElement("html")
    }
    if (head == null) {
      htmlEl.prependElement("head")
    }
    if (body == null) {
      htmlEl.appendElement("body")
    }
    // pull text nodes out of root, html, and head els, and push into body. non-text nodes are already taken care
    // of. do in inverse order to maintain text order.
    normaliseTextNodes(head)
    normaliseTextNodes(htmlEl)
    normaliseTextNodes(this)

    normaliseStructure("head", htmlEl)
    normaliseStructure("body", htmlEl)

    ensureMetaCharsetElement()
    this
  }

  // does not recurse.
  private def normaliseTextNodes(element: Element) {
    val toMove: mutable.Buffer[Node] = new mutable.ArrayBuffer[Node]
    for (node <- element.childNodes) {
      node match {
        case tn: TextNode if !tn.isBlank =>
          toMove.append(tn)
      }
    }

    for (i <- (toMove.length - 1).to(0, -1)) {
      val node: Node = toMove(i)
      element.removeChild(node)
      body.prependChild(new TextNode(" ", ""))
      body.prependChild(node)
    }
  }

  // merge multiple <head> or <body> contents into one, delete the remainder, and ensure they are owned by <html>
  private def normaliseStructure(tag: String, htmlEl: Element) {
    val elements: Elements = this.getElementsByTag(tag)
    // will always be available as created above if not existent
    val master: Element = elements.first()
    if (elements.size > 1) {
      // dupes, move contents to master
      val toMove: mutable.Buffer[Node] = new mutable.ArrayBuffer[Node]
      for (i <- 1 to (elements.size - 1)) {
        val dupe: Node = elements.get(i)
        dupe.childNodes.foreach(node => toMove.append(node))
        dupe.remove()
      }
      toMove.foreach(dupe => master.appendChild(dupe))

    }
    // ensure parented by <html>
    if (!(master.parent == htmlEl)) {
      htmlEl.appendChild(master) // includes remove()
    }
  }

  // fast method to get first by tag name, used for html, head, body finders
  private def findFirstElementByTagName(tag: String, node: Node): Option[Element] = {
    if (node.nodeName == tag) {
      return Some(node.asInstanceOf[Element])
    } else {
      for (child <- node.childNodes) {
        val found = findFirstElementByTagName(tag, child)
        if (found != None) {
          return found
        }
      }
    }
    None
  }

  override def outerHtml: String = {
    super.html // no outer wrapper tag
  }

  /**
   * Set the text of the <code>body</code> of this document. Any existing nodes within the body will be cleared.
   * @param text unencoded text
   * @return this document
   */
  override def text(text: String): Element = {
    body.text(text) // overridden to not nuke doc structure
    this
  }

  override def nodeName(): String = {
    "#document"
  }

  /**
   * Sets the charset used in this document. This method is equivalent
   * to {@link OutputSettings#charset(java.nio.charset.Charset)
     * OutputSettings.charset(Charset)} but in addition it updates the
   * charset / encoding element within the document.
   *
   * <p>This enables
   * {@link #updateMetaCharsetElement(boolean) meta charset update}.</p>
   *
   * <p>If there's no element with charset / encoding information yet it will
   * be created. Obsolete charset / encoding definitions are removed!</p>
   *
   * <p><b>Elements used:</b></p>
   *
   * <ul>
   * <li><b>Html:</b> <i>&lt;meta charset="CHARSET"&gt;</i></li>
   * <li><b>Xml:</b> <i>&lt;?xml version="1.0" encoding="CHARSET"&gt;</i></li>
   * </ul>
   *
   * @param charset Charset
   *
   * @see #updateMetaCharsetElement(boolean)
   * @see OutputSettings#charset(java.nio.charset.Charset)
   */
  def charset(charset: Charset) {
    updateMetaCharsetElement(true)
    outputSettings.charset(charset)
    ensureMetaCharsetElement()
  }

  /**
   * Returns the charset used in this document. This method is equivalent
   * to {@link OutputSettings#charset()}.
   *
   * @return Current Charset
   *
   * @see OutputSettings#charset()
   */
  def charset: Charset = {
    outputSettings.charset
  }

  /**
   * Sets whether the element with charset information in this document is
   * updated on changes through {@link #charset(java.nio.charset.Charset)
     * Document.charset(Charset)} or not.
   *
   * <p>If set to <tt>false</tt> <i>(default)</i> there are no elements
   * modified.</p>
   *
   * @param update If <tt>true</tt> the element updated on charset
   *               changes, <tt>false</tt> if not
   *
   * @see #charset(java.nio.charset.Charset)
   */
  def updateMetaCharsetElement(update: Boolean) {
    this.updateMetaCharsetVal = true
  }

  /**
   * Returns whether the element with charset information in this document is
   * updated on changes through {@link #charset(java.nio.charset.Charset)
     * Document.charset(Charset)} or not.
   *
   * @return Returns <tt>true</tt> if the element is updated on charset
   *         changes, <tt>false</tt> if not
   */
  def updateMetaCharsetElement(): Boolean = {
    updateMetaCharsetVal
  }

  override def clone(): Document = {
    val clone: Document = super.clone().asInstanceOf[Document]
    clone.outputSettingsVal = this.outputSettingsVal.clone
    clone
  }

  /**
   * Ensures a meta charset (html) or xml declaration (xml) with the current
   * encoding used. This only applies with
   * {@link #updateMetaCharsetElement(boolean) updateMetaCharset} set to
   * <tt>true</tt>, otherwise this method does nothing.
   *
   * <ul>
   * <li>An exsiting element gets updated with the current charset</li>
   * <li>If there's no element yet it will be inserted</li>
   * <li>Obsolete elements are removed</li>
   * </ul>
   *
   * <p><b>Elements used:</b></p>
   *
   * <ul>
   * <li><b>Html:</b> <i>&lt;meta charset="CHARSET"&gt;</i></li>
   * <li><b>Xml:</b> <i>&lt;?xml version="1.0" encoding="CHARSET"&gt;</i></li>
   * </ul>
   */
  private def ensureMetaCharsetElement() {
    if (updateMetaCharsetVal) {
      val syntax = outputSettings.syntax
      if (syntax eq OutputSettings.Syntax.html) {
        val metaCharset: Element = select("meta[charset]").first()
        if (metaCharset != null) {
          metaCharset.attr("charset", charset.displayName)
        } else {
          val headVal: Element = head
          if (headVal != null) {
            headVal.appendElement("meta").attr("charset", charset.displayName)
          }
        }
        select("meta[name=charset]").remove
      } else if (syntax == OutputSettings.Syntax.xml) {
        val node: Node = childNodes.head
        node match {
          case decl: XmlDeclaration =>
            if (decl.attr(XmlDeclaration.DECL_KEY) == "xml") {
              decl.attr("encoding", charset.displayName)
              val version: String = decl.attr("version")
              if (version != null) {
                decl.attr("version", "1.0")
              }
            } else {
              val declNew = new XmlDeclaration("xml", baseUri, false)
              declNew.attr("version", "1.0")
              declNew.attr("encoding", charset.displayName)
              prependChild(declNew)
            }
          case _ =>
            val decl: XmlDeclaration = new XmlDeclaration("xml", baseUri, false)
            decl.attr("version", "1.0")
            decl.attr("encoding", charset.displayName)
            prependChild(decl)
        }
      } else {
        // Unsupported syntax - nothing to do yet
      }
    }
  }

  /**
   * Get the document's current output settings.
   * @return the document's current output settings.
   */
  def outputSettings: Document.OutputSettings = outputSettingsVal

  /**
   * Set the document's output settings.
   * @param outputSettings new output settings.
   * @return this document, for chaining.
   */
  def outputSettings(outputSettings: Document.OutputSettings): Document = {
    notNull(outputSettings)
    this.outputSettingsVal = outputSettings
    this
  }

  def quirksMode(): Document.QuirksMode = {
    quirksModeVal
  }

  def quirksMode(quirksMode: Document.QuirksMode): Document = {
    this.quirksModeVal = quirksMode
    this
  }

}

object Document {

  /**
   * Create a valid, empty shell of a document, suitable for adding more elements to.
   * @param baseUri baseUri of document
   * @return document with html, head, and body elements.
   */
  def createShell(baseUri: String): Document = {
    notNull(baseUri)
    val doc: Document = new Document(baseUri)
    val html: Element = doc.appendElement("html")
    html.appendElement("head")
    html.appendElement("body")
    doc
  }

  object OutputSettings {

    /**
     * The output serialization syntax.
     */
    object Syntax extends Enumeration {

      val html, xml = Value
    }

    type Syntax = Syntax.Value
  }

  /**
   * A Document's output settings control the form of the text() and html() methods.
   */
  class OutputSettings extends Cloneable {

    import Entities._

    private var escapeModeVal: EscapeMode = BASE
    private var charsetVal: Charset = Charset.forName("UTF-8")
    private var charsetEncoder: CharsetEncoder = charsetVal.newEncoder
    private var prettyPrintVal: Boolean = true
    private var outlineVal: Boolean = false
    private var indentAmountVal: Int = 1
    private var syntaxVal: OutputSettings.Syntax = OutputSettings.Syntax.html

    /**
     * Get the document's current HTML escape mode: <code>base</code>, which provides a limited set of named HTML
     * entities and escapes other characters as numbered entities for maximum compatibility; or <code>extended</code>,
     * which uses the complete set of HTML named entities.
     * <p>
     * The default escape mode is <code>base</code>.
     * @return the document's current escape mode
     */
    def escapeMode: EscapeMode = escapeModeVal

    /**
     * Set the document's escape mode, which determines how characters are escaped when the output character set
     * does not support a given character:- using either a named or a numbered escape.
     * @param escapeMode the new escape mode to use
     * @return the document's output settings, for chaining
     */
    def escapeMode(escapeMode: EscapeMode): Document.OutputSettings = {
      this.escapeModeVal = escapeMode
      this
    }

    /**
     * Get the document's current output charset, which is used to control which characters are escaped when
     * generating HTML (via the <code>html()</code> methods), and which are kept intact.
     * <p>
     * Where possible (when parsing from a URL or File), the document's output charset is automatically set to the
     * input charset. Otherwise, it defaults to UTF-8.
     * @return the document's current charset.
     */
    def charset: Charset = charsetVal

    /**
     * Update the document's output charset.
     * @param charSet the new charset to use.
     * @return the document's output settings, for chaining
     */
    def charset(charSet: Charset): Document.OutputSettings = {
      this.charsetVal = charSet
      charsetEncoder = charSet.newEncoder
      this
    }

    /**
     * Update the document's output charset.
     * @param charSet the new charset (by name) to use.
     * @return the document's output settings, for chaining
     */
    def charset(charSet: String): Document.OutputSettings = {
      charset(Charset.forName(charSet))
      this
    }

    private[nodes] def encoder: CharsetEncoder = charsetEncoder

    /**
     * Get the document's current output syntax.
     * @return current syntax
     */
    def syntax: OutputSettings.Syntax = syntaxVal

    /**
     * Set the document's output syntax. Either <code>html</code>, with empty tags and boolean attributes (etc), or
     * <code>xml</code>, with self-closing tags.
     * @param syntaxIn serialization syntax
     * @return the document's output settings, for chaining
     */
    def syntax(syntaxIn: Document.OutputSettings.Syntax): Document.OutputSettings = {
      this.syntaxVal = syntaxIn
      this
    }

    /**
     * Get if pretty printing is enabled. Default is true. If disabled, the HTML output methods will not re-format
     * the output, and the output will generally look like the input.
     * @return if pretty printing is enabled.
     */
    def prettyPrint: Boolean = prettyPrintVal

    /**
     * Enable or disable pretty printing.
     * @param pretty new pretty print setting
     * @return this, for chaining
     */
    def prettyPrint(pretty: Boolean): Document.OutputSettings = {
      this.prettyPrintVal = pretty
      this
    }

    /**
     * Get if outline mode is enabled. Default is false. If enabled, the HTML output methods will consider
     * all tags as block.
     * @return if outline mode is enabled.
     */
    def outline: Boolean = outlineVal

    /**
     * Enable or disable HTML outline mode.
     * @param outlineMode new outline setting
     * @return this, for chaining
     */
    def outline(outlineMode: Boolean): Document.OutputSettings = {
      this.outlineVal = outlineMode
      this
    }

    /**
     * Get the current tag indent amount, used when pretty printing.
     * @return the current indent amount
     */
    def indentAmount: Int = indentAmountVal

    /**
     * Set the indent amount for pretty printing
     * @param indentAmountIn number of spaces to use for indenting each level. Must be >= 0.
     * @return this, for chaining
     */
    def indentAmount(indentAmountIn: Int): Document.OutputSettings = {
      isTrue(indentAmountIn >= 0)
      this.indentAmountVal = indentAmountIn
      this
    }

    override def clone: Document.OutputSettings = {
      var clone: Document.OutputSettings = null
      try {
        clone = super.clone.asInstanceOf[Document.OutputSettings]
      } catch {
        case e: CloneNotSupportedException =>
          throw new RuntimeException(e)
      }
      clone.charset(charset.name)
      clone.escapeModeVal = this.escapeModeVal
      clone
    }
  }

  object QuirksMode extends Enumeration {

    type QuirksMode = Value
    val noQuirks, quirks, limitedQuirks = Value
  }

  type QuirksMode = QuirksMode.QuirksMode

}
