package com.github.ligangty.scala.jsoup

import java.io.{File, IOException}

import com.github.ligangty.scala.jsoup.helper.{HttpConnection, DataUtil}
import com.github.ligangty.scala.jsoup.nodes.Document
import com.github.ligangty.scala.jsoup.parser.Parser

/**
 * The core public access point to the jsoup functionality.
 */
object Jsoup {

  /**
   * Parse HTML into a Document. The parser will make a sensible, balanced document tree out of any HTML.
   * @param html    HTML to parse
   * @param baseUri The URL where the HTML was retrieved from. Used to resolve relative URLs to absolute URLs, that occur
   *                before the HTML declares a {{{<base href>}}} tag.
   * @return sane HTML
   */
  def parse(html: String, baseUri: String): Document = {
    Parser.parse(html, baseUri)
  }

  /**
   * Parse HTML into a Document, using the provided Parser. You can provide an alternate parser, such as a simple XML
   * (non-HTML) parser.
   *
   * @param html    HTML to parse
   * @param baseUri The URL where the HTML was retrieved from. Used to resolve relative URLs to absolute URLs, that occur
   *                before the HTML declares a {{{<base href>}}} tag.
   * @param parser alternate [[Parser.xmlParser()]] parser to use.
   * @return sane HTML
   */
  def parse(html: String, baseUri: String, parser: Parser): Document = {
    parser.parseInput(html, baseUri)
  }

  /**
   * Parse HTML into a Document. As no base URI is specified, absolute URL detection relies on the HTML including a
   * {{{<base href>}}} tag.
   * @param html HTML to parse
   * @return sane HTML
   * @see [[parse(html: String, baseUri: String)]]
   */
  def parse(html: String): Document = {
    Parser.parse(html, "")
  }

  /**
   * Creates a new [[Connection]] to a URL. Use to fetch and parse a HTML page.
   * <p>
   * Use examples:
   * <ul>
   * <li><code>Document doc = Jsoup.connect("http://example.com").userAgent("Mozilla").data("name", "jsoup").get();</code></li>
   * <li><code>Document doc = Jsoup.connect("http://example.com").cookie("auth", "token").post();</code></li>
   * </ul>
   * @param url URL to connect to. The protocol must be { @code http} or { @code https}.
   * @return the connection. You can add data, cookies, and headers; set the user-agent, referrer, method; and then execute.
   */
  def connect(url: String): Connection = {
    HttpConnection.connect(url)
  }

  /**
   * Parse the contents of a file as HTML. The location of the file is used as the base URI to qualify relative URLs.

   * @param in          file to load HTML from
   * @param charsetName (optional) character set of file contents. Set to { @code null} to determine from { @code http-equiv} meta tag, if
   *                    present, or fall back to { @code UTF-8} (which is often safe to do).
   * @return sane HTML

   * @throws IOException if the file could not be found, or read, or if the charsetName is invalid.
   * @see [[parse(File, String, String)]]
   */
  @throws(classOf[IOException])
  def parse(in: File, charsetName: String): Document = {
    DataUtil.load(in, charsetName, in.getAbsolutePath)
  }

  /**
   * Creates a new {@link Connection} to a URL. Use to fetch and parse a HTML page.
   * <p>
   * Use examples:
   * <ul>
   * <li><code>Document doc = Jsoup.connect("http://example.com").userAgent("Mozilla").data("name", "jsoup").get();</code></li>
   * <li><code>Document doc = Jsoup.connect("http://example.com").cookie("auth", "token").post();</code></li>
   * </ul>
   * @param url URL to connect to. The protocol must be { @code http} or { @code https}.
   * @return the connection. You can add data, cookies, and headers; set the user-agent, referrer, method; and then execute.
   */
  def connect(url: String): Connection = {
    //todo not implemented
    null
  }
}
