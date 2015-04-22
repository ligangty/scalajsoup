package com.github.ligangty.scala.jsoup.parser

import com.github.ligangty.scala.jsoup.nodes.{Node, Element, Document}

/**
 * Parses HTML into a [[com.github.ligangty.scala.jsoup.nodes.Document]]. Generally best to use one of the  more convenient parse methods
 * in [[com.github.ligangty.scala.jsoup.Jsoup]].
 *
 * @constructor Create a new Parser, using the specified TreeBuilder
 * @param treeBuilder TreeBuilder to use to parse input into Documents.
 */
class Parser(var treeBuilder: TreeBuilder) {

  private var maxErrors: Int = Parser.DEFAULT_MAX_ERRORS
  private var errors: ParseErrorList = null

  def parseInput(html: String, baseUri: String): Document = {
    errors = if (isTrackErrors) {
      ParseErrorList.tracking(maxErrors)
    } else {
      ParseErrorList.noTracking
    }
    treeBuilder.parse(html, baseUri, errors)
  }

  // gets & sets
  /**
   * Get the TreeBuilder currently in use.
   * @return current TreeBuilder.
   */
  def getTreeBuilder: TreeBuilder = treeBuilder

  /**
   * Update the TreeBuilder used when parsing content.
   * @param treeBuilder current TreeBuilder
   * @return this, for chaining
   */
  def setTreeBuilder(treeBuilder: TreeBuilder): Parser = {
    this.treeBuilder = treeBuilder
    this
  }

  /**
   * Check if parse error tracking is enabled.
   * @return current track error state.
   */
  def isTrackErrors: Boolean = maxErrors > 0

  /**
   * Enable or disable parse error tracking for the next parse.
   * @param maxErrors the maximum number of errors to track. Set to 0 to disable.
   * @return this, for chaining
   */
  def setTrackErrors(maxErrors: Int): Parser = {
    this.maxErrors = maxErrors
    this
  }

  /**
   * Retrieve the parse errors, if any, from the last parse.
   * @return list of parse errors, up to the size of the maximum errors tracked.
   */
  def getErrors: Seq[ParseError] = errors.toSeq

}

object Parser {

  private val DEFAULT_MAX_ERRORS = 0

  // parse functions below
  /**
   * Parse HTML into a Document.
   *
   * @param html HTML to parse
   * @param baseUri base URI of document (i.e. original fetch location), for resolving relative URLs.
   *
   * @return parsed Document
   */
  def parse(html: String, baseUri: String): Document = {
    new HtmlTreeBuilder().parse(html, baseUri, ParseErrorList.noTracking)
  }

  /**
   * Parse a fragment of HTML into a list of nodes. The context element, if supplied, supplies parsing context.
   *
   * @param fragmentHtml the fragment of HTML to parse
   * @param context (optional) the element that this HTML fragment is being parsed for (i.e. for inner HTML). This
   *                provides stack context (for implicit element creation).
   * @param baseUri base URI of document (i.e. original fetch location), for resolving relative URLs.
   *
   * @return list of nodes parsed from the input HTML. Note that the context element, if supplied, is not modified.
   */
  def parseFragment(fragmentHtml: String, context: Element, baseUri: String): Seq[Node] = {
    new HtmlTreeBuilder().parseFragment(fragmentHtml, context, baseUri, ParseErrorList.noTracking)
  }

  /**
   * Parse a fragment of XML into a list of nodes.
   *
   * @param fragmentXml the fragment of XML to parse
   * @param baseUri base URI of document (i.e. original fetch location), for resolving relative URLs.
   * @return list of nodes parsed from the input XML.
   */
  def parseXmlFragment(fragmentXml: String, baseUri: String): Seq[Node] = {
    new XmlTreeBuilder().parseFragment(fragmentXml, baseUri, ParseErrorList.noTracking)
  }

  /**
   * Parse a fragment of HTML into the <code>body</code> of a Document.
   *
   * @param bodyHtml fragment of HTML
   * @param baseUri base URI of document (i.e. original fetch location), for resolving relative URLs.
   *
   * @return Document, with empty head, and HTML parsed into body
   */
  def parseBodyFragment(bodyHtml: String, baseUri: String): Document = {
    val doc: Document = Document.createShell(baseUri)
    val body: Element = doc.body
    val nodeList: Seq[Node] = parseFragment(bodyHtml, body, baseUri)
    val nodes: Array[Node] = nodeList.toArray // the node list gets modified when re-parented
    nodes.foreach(node => body.appendChild(node))
    doc
  }

  /**
   * Utility method to unescape HTML entities from a string
   * @param string HTML escaped string
   * @param inAttribute if the string is to be escaped in strict mode (as attributes are)
   * @return an unescaped string
   */
  def unescapeEntities(string: String, inAttribute: Boolean): String = {
    val tokeniser: Tokeniser = new Tokeniser(new CharacterReader(string), ParseErrorList.noTracking)
    tokeniser.unescapeEntities(inAttribute)
  }

  /**
   * @param bodyHtml HTML to parse
   * @param baseUri baseUri base URI of document (i.e. original fetch location), for resolving relative URLs.
   *
   * @return parsed Document
   * @deprecated Use { @link #parseBodyFragment} or { @link #parseFragment} instead.
   */
  def parseBodyFragmentRelaxed(bodyHtml: String, baseUri: String): Document = {
    parse(bodyHtml, baseUri)
  }

  // builders
  /**
   * Create a new HTML parser. This parser treats input as HTML5, and enforces the creation of a normalised document,
   * based on a knowledge of the semantics of the incoming tags.
   * @return a new HTML parser.
   */
  def htmlParser: Parser = {
    new Parser(new HtmlTreeBuilder)
  }

  /**
   * Create a new XML parser. This parser assumes no knowledge of the incoming tags and does not treat it as HTML,
   * rather creates a simple tree directly from the input.
   * @return a new simple XML parser.
   */
  def xmlParser: Parser = {
    new Parser(new XmlTreeBuilder)
  }
}
