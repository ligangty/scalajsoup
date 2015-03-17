package com.github.ligangty.scala.jsoup.nodes

import java.nio.charset.{CharsetEncoder, Charset}

import com.github.ligangty.scala.jsoup.helper.Validator
import com.github.ligangty.scala.jsoup.parser.Tag

/**
 * Created by gli on 15-3-10.
 */
class Document private(baseUri: String, location: String) extends Element(Tag("#root"), baseUri) {
  def this(baseUri: String) {
    this(baseUri, baseUri)
  }
}

object Document {

  class OutputSettings extends Cloneable {

    object Syntax extends Enumeration {
      type Syntax = Value
      val html, xml = Value
    }

    type Syntax = Syntax.Syntax

    import Entities._

    private var escapeModeVal: EscapeMode = BASE
    private var charsetVal: Charset = Charset.forName("UTF-8")
    private var charsetEncoder: CharsetEncoder = charsetVal.newEncoder
    private var prettyPrintVal: Boolean = true
    private var outlineVal: Boolean = false
    private var indentAmountVal: Int = 1
    private var syntaxVal: OutputSettings#Syntax = Syntax.html

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
    def syntax: OutputSettings#Syntax = syntaxVal

    /**
     * Set the document's output syntax. Either {@code html}, with empty tags and boolean attributes (etc), or
     * {@code xml}, with self-closing tags.
     * @param syntaxIn serialization syntax
     * @return the document's output settings, for chaining
     */
    def syntax(syntaxIn: Document.OutputSettings#Syntax): Document.OutputSettings = {
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
      return this
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
      Validator.isTrue(indentAmountIn >= 0)
      this.indentAmountVal = indentAmountIn
      this
    }

    override def clone: Document.OutputSettings = {
      var clone: Document.OutputSettings = null
      try {
        clone = super.clone.asInstanceOf[Document.OutputSettings]
      }
      catch {
        case e: CloneNotSupportedException => {
          throw new RuntimeException(e)
        }
      }
      clone.charset(charset.name)
      clone.escapeModeVal = this.escapeModeVal
      clone
    }
  }

}
