package com.github.ligangty.scala.jsoup.helper

import java.io.InputStream
import java.net.{SocketTimeoutException, MalformedURLException, URL}

import com.github.ligangty.scala.jsoup.Connection
import Validator._
import com.github.ligangty.scala.jsoup.Connection.Method.Method
import com.github.ligangty.scala.jsoup.Connection.{KeyVal, Request, Response}
import com.github.ligangty.scala.jsoup.nodes.Document
import com.github.ligangty.scala.jsoup.parser.Parser

import scala.collection.mutable

class HttpConnection extends Connection {
  /**
   * Set the request URL to fetch. The protocol must be HTTP or HTTPS.
   * @param url URL to connect to
   * @return this Connection, for chaining
   */
  override def url(url: URL): Connection = ???

  /**
   * Provide an alternate parser to use when parsing the response to a Document.
   * @param parser alternate parser
   * @return this Connection, for chaining
   */
  override def parser(parser: Parser): Connection = ???

  /**
   * Disable/enable TSL certificates validation for HTTPS requests.
   * <p/>
   * By default this is <b>true</b>; all
   * connections over HTTPS perform normal validation of certificates, and will abort requests if the provided
   * certificate does not validate.
   * <p/>
   * Some servers use expired, self-generated certificates; or your JDK may not
   * support SNI hosts. In which case, you may want to enable this setting.
   * <p/> <b>Be careful</b> and understand why you need to disable these validations.
   * @param value if should validate TSL (SSL) certificates. <b>true</b> by default.
   * @return this Connection, for chaining
   */
  override def validateTLSCertificates(value: Boolean): Connection = ???

  /**
   * Execute the request as a GET, and parse the result.
   * @return parsed Document
   * @throws MalformedURLException if the request URL is not a HTTP or HTTPS URL, or is otherwise malformed
   * @throws HttpStatusException if the response is not OK and HTTP response errors are not ignored
   * @throws UnsupportedMimeTypeException if the response mime type is not supported and those errors are not ignored
   * @throws SocketTimeoutException if the connection times out
   * @throws IOException on error
   */
  override def get: Document = ???

  /**
   * Add a request data parameter. Request parameters are sent in the request query string for GETs, and in the
   * request body for POSTs. A request may have multiple values of the same name.
   * @param key data key
   * @param value data value
   * @return this Connection, for chaining
   */
  override def data(key: String, value: String): Connection = ???

  /**
   * Add an input stream as a request data paramater. For GETs, has no effect, but for POSTS this will upload the
   * input stream.
   * @param key data key (form item name)
   * @param filename the name of the file to present to the remove server. Typically just the name, not path,
   *                 component.
   * @param inputStream the input stream to upload, that you probably obtained from a { @link java.io.FileInputStream}.
   *                    You must close the InputStream in a { @code finally} block.
   * @return this Connections, for chaining
   */
  override def data(key: String, filename: String, inputStream: InputStream): Connection = ???

  /**
   * Adds all of the supplied data to the request data parameters
   * @param data collection of data parameters
   * @return this Connection, for chaining
   */
  override def data(data: Traversable[KeyVal]): Connection = ???

  /**
   * Adds all of the supplied data to the request data parameters
   * @param data map of data parameters
   * @return this Connection, for chaining
   */
  override def data(data: Map[String, String]): Connection = ???

  /**
   * Add a number of request data parameters. Multiple parameters may be set at once, e.g.: <code>.data("name",
   * "jsoup", "language", "Java", "language", "English");</code> creates a query string like:
   * <code>?name=jsoup&language=Java&language=English</code>
   * @param keyvals a set of key value pairs.
   * @return this Connection, for chaining
   */
  override def data(keyvals: String*): Connection = ???

  /**
   * Add a number of request data parameters. Multiple parameters may be set at once, e.g.: <code>.data("name",
   * "jsoup", "language", "Java", "language", "English");</code> creates a query string like:
   * <code>?name=jsoup&language=Java&language=English</code>
   * @param keyvals a set of key value pairs.
   * @return this Connection, for chaining
   */
  override def data(keyvals: Array[String]): Connection = ???

  /**
   * Set the request URL to fetch. The protocol must be HTTP or HTTPS.
   * @param url URL to connect to
   * @return this Connection, for chaining
   */
  override def url(url: String): Connection = ???

  /**
   * Execute the request.
   * @return a response object
   * @throws MalformedURLException if the request URL is not a HTTP or HTTPS URL, or is otherwise malformed
   * @throws HttpStatusException if the response is not OK and HTTP response errors are not ignored
   * @throws UnsupportedMimeTypeException if the response mime type is not supported and those errors are not ignored
   * @throws SocketTimeoutException if the connection times out
   * @throws IOException on error
   */
  override def execute: Response = ???

  /**
   * Set the request method to use, GET or POST. Default is GET.
   * @param method HTTP request method
   * @return this Connection, for chaining
   */
  override def method(method: Method): Connection = ???

  /**
   * Configures the connection to (not) follow server redirects. By default this is <b>true</b>.
   * @param followRedirects true if server redirects should be followed.
   * @return this Connection, for chaining
   */
  override def followRedirects(followRedirects: Boolean): Connection = ???

  /**
   * Set a request header.
   * @param name header name
   * @param value header value
   * @return this Connection, for chaining
   * @see org.jsoup.Connection.Request#headers()
   */
  override def header(name: String, value: String): Connection = ???

  /**
   * Set the request referrer (aka "referer") header.
   * @param referrer referrer to use
   * @return this Connection, for chaining
   */
  override def referrer(referrer: String): Connection = ???

  /**
   * Configures the connection to not throw exceptions when a HTTP error occurs. (4xx - 5xx, e.g. 404 or 500). By
   * default this is <b>false</b>; an IOException is thrown if an error is encountered. If set to <b>true</b>, the
   * response is populated with the error body, and the status message will reflect the error.
   * @param ignoreHttpErrors - false (default) if HTTP errors should be ignored.
   * @return this Connection, for chaining
   */
  override def ignoreHttpErrors(ignoreHttpErrors: Boolean): Connection = ???

  /**
   * Set a cookie to be sent in the request.
   * @param name name of cookie
   * @param value value of cookie
   * @return this Connection, for chaining
   */
  override def cookie(name: String, value: String): Connection = ???

  /**
   * Execute the request as a POST, and parse the result.
   * @return parsed Document
   * @throws MalformedURLException if the request URL is not a HTTP or HTTPS URL, or is otherwise malformed
   * @throws HttpStatusException if the response is not OK and HTTP response errors are not ignored
   * @throws UnsupportedMimeTypeException if the response mime type is not supported and those errors are not ignored
   * @throws SocketTimeoutException if the connection times out
   * @throws IOException on error
   */
  override def post: Document = ???

  /**
   * Ignore the document's Content-Type when parsing the response. By default this is <b>false</b>, an unrecognised
   * content-type will cause an IOException to be thrown. (This is to prevent producing garbage by attempting to parse
   * a JPEG binary image, for example.) Set to true to force a parse attempt regardless of content type.
   * @param ignoreContentType set to true if you would like the content type ignored on parsing the response into a
   *                          Document.
   * @return this Connection, for chaining
   */
  override def ignoreContentType(ignoreContentType: Boolean): Connection = ???

  /**
   * Adds each of the supplied cookies to the request.
   * @param cookies map of cookie name -> value pairs
   * @return this Connection, for chaining
   */
  override def cookies(cookies: Map[String, String]): Connection = ???

  /**
   * Set the maximum bytes to read from the (uncompressed) connection into the body, before the connection is closed,
   * and the input truncated. The default maximum is 1MB. A max size of zero is treated as an infinite amount (bounded
   * only by your patience and the memory available on your machine).
   * @param bytes number of bytes to read from the input before truncating
   * @return this Connection, for chaining
   */
  override def maxBodySize(bytes: Int): Connection = ???

  /**
   * Set the connection's request
   * @param request new request object
   * @return this Connection, for chaining
   */
  override def request(request: Request): Connection = ???

  /**
   * Get the request object associated with this connection
   * @return request
   */
  override def request: Request = ???

  /**
   * Set the request timeouts (connect and read). If a timeout occurs, an IOException will be thrown. The default
   * timeout is 3 seconds (3000 millis). A timeout of zero is treated as an infinite timeout.
   * @param millis number of milliseconds (thousandths of a second) before timing out connects or reads.
   * @return this Connection, for chaining
   */
  override def timeout(millis: Int): Connection = ???

  /**
   * Set the connection's response
   * @param response new response
   * @return this Connection, for chaining
   */
  override def response(response: Response): Connection = ???

  /**
   * Get the response, once the request has been executed
   * @return response
   */
  override def response: Response = ???

  /**
   * Set the request user-agent header.
   * @param userAgent user-agent to use
   * @return this Connection, for chaining
   */
  override def userAgent(userAgent: String): Connection = ???
}

object HttpConnection {

  private[HttpConnection] abstract class Base[T <: Connection.Base[T]] protected(n:Unit=()) extends Connection.Base[T] {
    protected[helper] var urlVal: URL = null
    protected[helper] var methodVal: Connection.Method.Method = null
    protected[helper] var headersMap: mutable.Map[String, String] = null
    protected[helper] var cookiesMap: mutable.Map[String, String] = null

    private def this() {
      this(())
      this.headersMap = new mutable.LinkedHashMap[String, String]
      this.cookiesMap = new mutable.LinkedHashMap[String, String]
    }

    def url: URL = {
      urlVal
    }

    def url(url: URL): T = {
      notNull(url, "URL must not be null")
      this.urlVal = url
      this.asInstanceOf[T]
    }

    def method: Connection.Method.Method = {
      methodVal
    }

    def method(method: Connection.Method.Method): T = {
      notNull(method, "Method must not be null")
      this.methodVal = method
      this.asInstanceOf[T]
    }

    def header(name: String): String = {
      notNull(name, "Header name must not be null")
      getHeaderCaseInsensitive(name)
    }

    def header(name: String, value: String): T = {
      notEmpty(name, "Header name must not be empty")
      notNull(value, "Header value must not be null")
      removeHeader(name)
      headersMap.put(name, value)
      this.asInstanceOf[T]
    }

    def hasHeader(name: String): Boolean = {
      notEmpty(name, "Header name must not be empty")
      getHeaderCaseInsensitive(name) != null
    }

    /**
     * Test if the request has a header with this value (case insensitive).
     */
    def hasHeaderWithValue(name: String, value: String): Boolean = {
      hasHeader(name) && header(name).equalsIgnoreCase(value)
    }

    def removeHeader(name: String): T = {
      notEmpty(name, "Header name must not be empty")
      val entry = scanHeaders(name)
      if (entry != null) headersMap.remove(entry._1)
      this.asInstanceOf[T]
    }

    def headers: Map[String, String] = headersMap.toMap

    private def getHeaderCaseInsensitive(name: String): String = {
      notNull(name, "Header name must not be null")
      var value: String = headersMap(name)
      if (value == null) value = headersMap(name.toLowerCase)
      if (value == null) {
        val entry = scanHeaders(name)
        if (entry != null) value = entry._2
      }
      value
    }

    private def scanHeaders(name: String): Tuple2[String, String] = {
      val lc: String = name.toLowerCase
      for (entry <- headersMap) {
        if (entry._1.toLowerCase == lc) return entry
      }
      null
    }

    def cookie(name: String): String = {
      notEmpty(name, "Cookie name must not be empty")
      cookiesMap(name)
    }

    def cookie(name: String, value: String): T = {
      notEmpty(name, "Cookie name must not be empty")
      notNull(value, "Cookie value must not be null")
      cookiesMap.put(name, value)
      this.asInstanceOf[T]
    }

    def hasCookie(name: String): Boolean = {
      notEmpty(name, "Cookie name must not be empty")
      cookiesMap.keys.exists(_ == name)
    }

    def removeCookie(name: String): T = {
      notEmpty(name, "Cookie name must not be empty")
      cookiesMap.remove(name)
      this.asInstanceOf[T]
    }

    def cookies: Map[String, String] = cookiesMap.toMap
  }

  object KeyVal {
    def create(key: String, value: String): HttpConnection.KeyVal = {
      new HttpConnection.KeyVal().key(key).value(value)
    }

    def create(key: String, filename: String, stream: InputStream): HttpConnection.KeyVal = {
      new HttpConnection.KeyVal().key(key).value(filename).inputStream(stream)
    }
  }

  class Request private(n:Unit=()) extends HttpConnection.Base[Connection.Request] with Connection.Request {
    private var timeoutMilliseconds: Int = 0
    private var maxBodySizeBytes: Int = 0
    private var followRedirectsVal: Boolean = false
    private var dataVal: mutable.Buffer[Connection.KeyVal] = null
    private var ignoreHttpErrorsVal: Boolean = false
    private var ignoreContentTypeVal: Boolean = false
    private var parserVal: Parser = null
    private var validateTSLCertificatesVal: Boolean = true

    private def this() {
      this(())
      timeoutMilliseconds = 3000
      maxBodySizeBytes = 1024 * 1024
      followRedirectsVal = true
      dataVal = new mutable.ArrayBuffer[Connection.KeyVal]
      this.methodVal = Connection.Method.GET()
      headersMap.put("Accept-Encoding", "gzip")
      parserVal = Parser.htmlParser
    }

    def timeout: Int = {
      timeoutMilliseconds
    }

    def timeout(millis: Int): HttpConnection.Request = {
      isTrue(millis >= 0, "Timeout milliseconds must be 0 (infinite) or greater")
      timeoutMilliseconds = millis
      this
    }

    def maxBodySize: Int = {
      maxBodySizeBytes
    }

    def maxBodySize(bytes: Int): Connection.Request = {
      isTrue(bytes >= 0, "maxSize must be 0 (unlimited) or larger")
      maxBodySizeBytes = bytes
      this
    }

    def followRedirects: Boolean = {
      followRedirectsVal
    }

    def followRedirects(followRedirects: Boolean): Connection.Request = {
      this.followRedirectsVal = followRedirects
      this
    }

    def ignoreHttpErrors: Boolean = {
      ignoreHttpErrorsVal
    }

    def validateTLSCertificates: Boolean = {
      validateTSLCertificatesVal
    }

    def validateTLSCertificates(value: Boolean) {
      validateTSLCertificatesVal = value
    }

    def ignoreHttpErrors(ignoreHttpErrors: Boolean): Connection.Request = {
      this.ignoreHttpErrorsVal = ignoreHttpErrors
      this
    }

    def ignoreContentType: Boolean = {
      ignoreContentTypeVal
    }

    def ignoreContentType(ignoreContentType: Boolean): Connection.Request = {
      this.ignoreContentTypeVal = ignoreContentType
      this
    }

    def data(keyval: Connection.KeyVal): HttpConnection.Request = {
      notNull(keyval, "Key val must not be null")
      dataVal.append(keyval)
      this
    }

    override def data: Seq[Connection.KeyVal] = {
      dataVal.toSeq
    }

    def parser(parser: Parser): HttpConnection.Request = {
      this.parserVal = parser
      this
    }

    def parser: Parser = {
      parserVal
    }
  }

  class KeyVal private[HttpConnection]() extends Connection.KeyVal {
    private var keyVal: String = null
    private var valueVal: String = null
    private var stream: InputStream = null


    def key(key: String): HttpConnection.KeyVal = {
      notEmpty(key, "Data keyVal must not be empty")
      this.keyVal = key
      this
    }

    def key: String = {
      keyVal
    }

    def value(value: String): HttpConnection.KeyVal = {
      notNull(value, "Data valueVal must not be null")
      this.valueVal = value
      this
    }

    def value: String = {
      valueVal
    }

    def inputStream(inputStream: InputStream): HttpConnection.KeyVal = {
      notNull(valueVal, "Data input stream must not be null")
      this.stream = inputStream
      this
    }

    def inputStream: InputStream = {
      stream
    }

    def hasInputStream: Boolean = {
      stream != null
    }

    override def toString: String = {
      keyVal + "=" + valueVal
    }
  }

}
