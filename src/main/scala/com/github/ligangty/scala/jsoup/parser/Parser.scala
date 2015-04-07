package com.github.ligangty.scala.jsoup.parser

import com.github.ligangty.scala.jsoup.nodes.Document

/**
 * Parses HTML into a [[com.github.ligangty.scala.jsoup.nodes.Document]]. Generally best to use one of the  more convenient parse methods
 * in [[com.github.ligangty.scala.jsoup.Jsoup]].
 */
class Parser private() {
  private var treeBuilder: TreeBuilder = null
  private var maxErrors: Int = Parser.DEFAULT_MAX_ERRORS
  private var errors: ParseErrorList = null

  /**
   * Create a new Parser, using the specified TreeBuilder
   * @param treeBuilder TreeBuilder to use to parse input into Documents.
   */
  def this(treeBuilder: TreeBuilder) {
    this()
    this.treeBuilder = treeBuilder
  }

  def parseInput(html: String, baseUri: String): Document = {
    errors = if (isTrackErrors) ParseErrorList.tracking(maxErrors) else ParseErrorList.noTracking
    val doc: Document = treeBuilder.parse(html, baseUri, errors)
    doc
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


  // static parse functions below
  /**
   * Parse HTML into a Document.
   *
   * @param html HTML to parse
   * @param baseUri base URI of document (i.e. original fetch location), for resolving relative URLs.
   *
   * @return parsed Document
   */
  def parse(html: String, baseUri: String): Document = {
    val treeBuilder: TreeBuilder = new HtmlTreeBuilder
    treeBuilder.parse(html, baseUri, ParseErrorList.noTracking)
  }
}

object Parser {
  private val DEFAULT_MAX_ERRORS = 0

  def htmlParser: Parser = {
    new Parser(new HtmlTreeBuilder)
  }
}
