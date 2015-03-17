package com.github.ligangty.scala.jsoup.nodes

import java.nio.charset.{CharsetEncoder, Charset}

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

  class OutputSettings {

    object Syntax extends Enumeration {
      type Syntax = Value
      val html, xml = Value
    }

    import Entities._

    private var escapeModeValue: EscapeMode = BASE
    private val charset: Charset = Charset.forName("UTF-8")
    private val charsetEncoder: CharsetEncoder = charset.newEncoder
    private val prettyPrint: Boolean = true
    private val outline: Boolean = false
    private val indentAmount: Int = 1
    private val syntax = Syntax.html

    /**
     * Get the document's current HTML escape mode: <code>base</code>, which provides a limited set of named HTML
     * entities and escapes other characters as numbered entities for maximum compatibility; or <code>extended</code>,
     * which uses the complete set of HTML named entities.
     * <p>
     * The default escape mode is <code>base</code>.
     * @return the document's current escape mode
     */
    def escapeMode: EscapeMode = escapeModeValue

    /**
     * Set the document's escape mode, which determines how characters are escaped when the output character set
     * does not support a given character:- using either a named or a numbered escape.
     * @param escapeMode the new escape mode to use
     * @return the document's output settings, for chaining
     */
    def escapeMode(escapeMode: EscapeMode): Document.OutputSettings = {
      this.escapeModeValue = escapeMode
      this
    }
  }

}
