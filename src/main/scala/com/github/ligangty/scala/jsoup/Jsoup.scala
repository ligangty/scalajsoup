package com.github.ligangty.scala.jsoup

import java.io.{InputStream, File, IOException}
import java.net.URL

import com.github.ligangty.scala.jsoup.helper.{HttpConnection, DataUtil}
import com.github.ligangty.scala.jsoup.nodes.Document
import com.github.ligangty.scala.jsoup.parser.Parser
import com.github.ligangty.scala.jsoup.safety.{Whitelist, Cleaner}

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
   * Parse the contents of a file as HTML.
   *
   * @param in          file to load HTML from
   * @param charsetName (optional) character set of file contents. Set to { @code null} to determine from { @code http-equiv} meta tag, if
     present, or fall back to { @code UTF-8} (which is often safe to do).
   * @param baseUri     The URL where the HTML was retrieved from, to resolve relative links against.
   * @return sane HTML
   *
   * @throws IOException if the file could not be found, or read, or if the charsetName is invalid.
   */
  @throws(classOf[IOException])
  def parse(in: File, charsetName: String, baseUri: String): Document = {
    DataUtil.load(in, charsetName, baseUri)
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
   * Read an input stream, and parse it to a Document.
   *
   * @param in          input stream to read. Make sure to close it after parsing.
   * @param charsetName (optional) character set of file contents. Set to { @code null} to determine from { @code http-equiv} meta tag, if
   *                    present, or fall back to { @code UTF-8} (which is often safe to do).
   * @param baseUri     The URL where the HTML was retrieved from, to resolve relative links against.
   * @return sane HTML
   *
   * @throws IOException if the file could not be found, or read, or if the charsetName is invalid.
   */
  @throws(classOf[IOException])
  def parse(in: InputStream, charsetName: String, baseUri: String): Document = {
    DataUtil.load(in, charsetName, baseUri)
  }

  /**
   * Read an input stream, and parse it to a Document. You can provide an alternate parser, such as a simple XML
   * (non-HTML) parser.
   *
   * @param in          input stream to read. Make sure to close it after parsing.
   * @param charsetName (optional) character set of file contents. Set to { @code null} to determine from { @code http-equiv} meta tag, if
   *                    present, or fall back to { @code UTF-8} (which is often safe to do).
   * @param baseUri     The URL where the HTML was retrieved from, to resolve relative links against.
   * @param parser alternate { @link Parser#xmlParser() parser} to use.
   * @return sane HTML
   *
   * @throws IOException if the file could not be found, or read, or if the charsetName is invalid.
   */
  @throws(classOf[IOException])
  def parse(in: InputStream, charsetName: String, baseUri: String, parser: Parser): Document = {
    DataUtil.load(in, charsetName, baseUri, parser)
  }

  /**
   * Parse a fragment of HTML, with the assumption that it forms the {@code body} of the HTML.
   *
   * @param bodyHtml body HTML fragment
   * @param baseUri  URL to resolve relative URLs against.
   * @return sane HTML document
   *
   * @see Document#body()
   */
  def parseBodyFragment(bodyHtml: String, baseUri: String): Document = {
    Parser.parseBodyFragment(bodyHtml, baseUri)
  }

  /**
   * Parse a fragment of HTML, with the assumption that it forms the {@code body} of the HTML.
   *
   * @param bodyHtml body HTML fragment
   * @return sane HTML document
   *
   * @see Document#body()
   */
  def parseBodyFragment(bodyHtml: String): Document = {
    Parser.parseBodyFragment(bodyHtml, "")
  }

  /**
   * Fetch a URL, and parse it as HTML. Provided for compatibility; in most cases use {@link #connect(String)} instead.
   * <p>
   * The encoding character set is determined by the content-type header or http-equiv meta tag, or falls back to {@code UTF-8}.
   *
   * @param url           URL to fetch (with a GET). The protocol must be { @code http} or { @code https}.
   * @param timeoutMillis Connection and read timeout, in milliseconds. If exceeded, IOException is thrown.
   * @return The parsed HTML.
   *
   * @throws java.net.MalformedURLException if the request URL is not a HTTP or HTTPS URL, or is otherwise malformed
   * @throws HttpStatusException if the response is not OK and HTTP response errors are not ignored
   * @throws UnsupportedMimeTypeException if the response mime type is not supported and those errors are not ignored
   * @throws java.net.SocketTimeoutException if the connection times out
   * @throws IOException if a connection or read error occurs
   *
   * @see #connect(String)
   */
  @throws(classOf[IOException])
  def parse(url: URL, timeoutMillis: Int): Document = {
    val con: Connection = HttpConnection.connect(url)
    con.timeout(timeoutMillis)
    con.get
  }

  /**
   * Get safe HTML from untrusted input HTML, by parsing input HTML and filtering it through a white-list of permitted
   * tags and attributes.
   *
   * @param bodyHtml  input untrusted HTML (body fragment)
   * @param baseUri   URL to resolve relative URLs against
   * @param whitelist white-list of permitted HTML elements
   * @return safe HTML (body fragment)
   *
   * @see Cleaner#clean(Document)
   */
  def clean(bodyHtml: String, baseUri: String, whitelist: Whitelist): String = {
    val dirty: Document = parseBodyFragment(bodyHtml, baseUri)
    val cleaner: Cleaner = new Cleaner(whitelist)
    val clean: Document = cleaner.clean(dirty)
    clean.body.html
  }

  /**
   * Get safe HTML from untrusted input HTML, by parsing input HTML and filtering it through a white-list of permitted
   * tags and attributes.
   *
   * @param bodyHtml  input untrusted HTML (body fragment)
   * @param whitelist white-list of permitted HTML elements
   * @return safe HTML (body fragment)
   *
   * @see Cleaner#clean(Document)
   */
  def clean(bodyHtml: String, whitelist: Whitelist): String = {
    clean(bodyHtml, "", whitelist)
  }

  /**
   * Get safe HTML from untrusted input HTML, by parsing input HTML and filtering it through a white-list of
   * permitted
   * tags and attributes.
   *
   * @param bodyHtml input untrusted HTML (body fragment)
   * @param baseUri URL to resolve relative URLs against
   * @param whitelist white-list of permitted HTML elements
   * @param outputSettings document output settings; use to control pretty-printing and entity escape modes
   * @return safe HTML (body fragment)
   * @see Cleaner#clean(Document)
   */
  def clean(bodyHtml: String, baseUri: String, whitelist: Whitelist, outputSettings: Document.OutputSettings): String = {
    val dirty: Document = parseBodyFragment(bodyHtml, baseUri)
    val cleaner: Cleaner = new Cleaner(whitelist)
    val clean: Document = cleaner.clean(dirty)
    clean.outputSettings(outputSettings)
    clean.body.html
  }

  /**
   * Test if the input HTML has only tags and attributes allowed by the Whitelist. Useful for form validation. The input HTML should
   * still be run through the cleaner to set up enforced attributes, and to tidy the output.
   * @param bodyHtml HTML to test
   * @param whitelist whitelist to test against
   * @return true if no tags or attributes were removed; false otherwise
   * @see #clean(String, org.jsoup.safety.Whitelist)
   */
  def isValid(bodyHtml: String, whitelist: Whitelist): Boolean = {
    val dirty: Document = parseBodyFragment(bodyHtml, "")
    val cleaner: Cleaner = new Cleaner(whitelist)
    cleaner.isValid(dirty)
  }

}
