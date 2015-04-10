package com.github.ligangty.scala.jsoup

import com.github.ligangty.scala.jsoup.nodes._

import java.io.IOException
import java.io.InputStream
import java.net.URL
import java.util.Collection

import com.github.ligangty.scala.jsoup.parser.Parser

import scala.collection.mutable

/**
 * A Connection provides a convenient interface to fetch content from the web, and parse them into Documents.
 * <p/>
 * To get a new Connection, use [[Jsoup.connect(String)]]. Connections contain [[Connection.Request]]
 * and [[Connection.Response]] objects. The request objects are reusable as prototype requests.
 * <p/>
 * Request configuration can be made using either the shortcut methods in Connection (e.g. [[userAgent(String)]]),
 * or by methods in the Connection.Request object directly. All request configuration must be made before the request is
 * executed.
 */
trait Connection {

  /**
   * Set the request URL to fetch. The protocol must be HTTP or HTTPS.
   * @param url URL to connect to
   * @return this Connection, for chaining
   */
  def url(url: URL): Connection

  /**
   * Set the request URL to fetch. The protocol must be HTTP or HTTPS.
   * @param url URL to connect to
   * @return this Connection, for chaining
   */
  def url(url: String): Connection

  /**
   * Set the request user-agent header.
   * @param userAgent user-agent to use
   * @return this Connection, for chaining
   */
  def userAgent(userAgent: String): Connection

  /**
   * Set the request timeouts (connect and read). If a timeout occurs, an IOException will be thrown. The default
   * timeout is 3 seconds (3000 millis). A timeout of zero is treated as an infinite timeout.
   * @param millis number of milliseconds (thousandths of a second) before timing out connects or reads.
   * @return this Connection, for chaining
   */
  def timeout(millis: Int): Connection

  /**
   * Set the maximum bytes to read from the (uncompressed) connection into the body, before the connection is closed,
   * and the input truncated. The default maximum is 1MB. A max size of zero is treated as an infinite amount (bounded
   * only by your patience and the memory available on your machine).
   * @param bytes number of bytes to read from the input before truncating
   * @return this Connection, for chaining
   */
  def maxBodySize(bytes: Int): Connection

  /**
   * Set the request referrer (aka "referer") header.
   * @param referrer referrer to use
   * @return this Connection, for chaining
   */
  def referrer(referrer: String): Connection

  /**
   * Configures the connection to (not) follow server redirects. By default this is <b>true</b>.
   * @param followRedirects true if server redirects should be followed.
   * @return this Connection, for chaining
   */
  def followRedirects(followRedirects: Boolean): Connection

  /**
   * Set the request method to use, GET or POST. Default is GET.
   * @param method HTTP request method
   * @return this Connection, for chaining
   */
  def method(method: Connection.Method.Method): Connection

  /**
   * Configures the connection to not throw exceptions when a HTTP error occurs. (4xx - 5xx, e.g. 404 or 500). By
   * default this is <b>false</b>; an IOException is thrown if an error is encountered. If set to <b>true</b>, the
   * response is populated with the error body, and the status message will reflect the error.
   * @param ignoreHttpErrors - false (default) if HTTP errors should be ignored.
   * @return this Connection, for chaining
   */
  def ignoreHttpErrors(ignoreHttpErrors: Boolean): Connection

  /**
   * Ignore the document's Content-Type when parsing the response. By default this is <b>false</b>, an unrecognised
   * content-type will cause an IOException to be thrown. (This is to prevent producing garbage by attempting to parse
   * a JPEG binary image, for example.) Set to true to force a parse attempt regardless of content type.
   * @param ignoreContentType set to true if you would like the content type ignored on parsing the response into a
   *                          Document.
   * @return this Connection, for chaining
   */
  def ignoreContentType(ignoreContentType: Boolean): Connection

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
  def validateTLSCertificates(value: Boolean): Connection

  /**
   * Add a request data parameter. Request parameters are sent in the request query string for GETs, and in the
   * request body for POSTs. A request may have multiple values of the same name.
   * @param key data key
   * @param value data value
   * @return this Connection, for chaining
   */
  def data(key: String, value: String): Connection

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
  def data(key: String, filename: String, inputStream: InputStream): Connection

  /**
   * Adds all of the supplied data to the request data parameters
   * @param data collection of data parameters
   * @return this Connection, for chaining
   */
  def data(data: Traversable[Connection.KeyVal]): Connection

  /**
   * Adds all of the supplied data to the request data parameters
   * @param data map of data parameters
   * @return this Connection, for chaining
   */
  def data(data: Map[String, String]): Connection

  /**
   * Add a number of request data parameters. Multiple parameters may be set at once, e.g.: <code>.data("name",
   * "jsoup", "language", "Java", "language", "English");</code> creates a query string like:
   * <code>?name=jsoup&language=Java&language=English</code>
   * @param keyvals a set of key value pairs.
   * @return this Connection, for chaining
   */
  def data(keyvals: String*): Connection

  /**
   * Set a request header.
   * @param name header name
   * @param value header value
   * @return this Connection, for chaining
   * @see org.jsoup.Connection.Request#headers()
   */
  def header(name: String, value: String): Connection

  /**
   * Set a cookie to be sent in the request.
   * @param name name of cookie
   * @param value value of cookie
   * @return this Connection, for chaining
   */
  def cookie(name: String, value: String): Connection

  /**
   * Adds each of the supplied cookies to the request.
   * @param cookies map of cookie name -> value pairs
   * @return this Connection, for chaining
   */
  def cookies(cookies: Map[String, String]): Connection

  /**
   * Provide an alternate parser to use when parsing the response to a Document.
   * @param parser alternate parser
   * @return this Connection, for chaining
   */
  def parser(parser: Parser): Connection

  /**
   * Execute the request as a GET, and parse the result.
   * @return parsed Document
   * @throws java.net.MalformedURLException if the request URL is not a HTTP or HTTPS URL, or is otherwise malformed
   * @throws HttpStatusException if the response is not OK and HTTP response errors are not ignored
   * @throws UnsupportedMimeTypeException if the response mime type is not supported and those errors are not ignored
   * @throws java.net.SocketTimeoutException if the connection times out
   * @throws IOException on error
   */
  @throws(classOf[IOException])
  def get: Document

  /**
   * Execute the request as a POST, and parse the result.
   * @return parsed Document
   * @throws java.net.MalformedURLException if the request URL is not a HTTP or HTTPS URL, or is otherwise malformed
   * @throws HttpStatusException if the response is not OK and HTTP response errors are not ignored
   * @throws UnsupportedMimeTypeException if the response mime type is not supported and those errors are not ignored
   * @throws java.net.SocketTimeoutException if the connection times out
   * @throws IOException on error
   */
  @throws(classOf[IOException])
  def post: Document

  /**
   * Execute the request.
   * @return a response object
   * @throws java.net.MalformedURLException if the request URL is not a HTTP or HTTPS URL, or is otherwise malformed
   * @throws HttpStatusException if the response is not OK and HTTP response errors are not ignored
   * @throws UnsupportedMimeTypeException if the response mime type is not supported and those errors are not ignored
   * @throws java.net.SocketTimeoutException if the connection times out
   * @throws IOException on error
   */
  @throws(classOf[IOException])
  def execute: Connection.Response

  /**
   * Get the request object associated with this connection
   * @return request
   */
  def request: Connection.Request

  /**
   * Set the connection's request
   * @param request new request object
   * @return this Connection, for chaining
   */
  def request(request: Connection.Request): Connection

  /**
   * Get the response, once the request has been executed
   * @return response
   */
  def response: Connection.Response

  /**
   * Set the connection's response
   * @param response new response
   * @return this Connection, for chaining
   */
  def response(response: Connection.Response): Connection

}

object Connection {

  object Method {

    def valueOf(value: String): Method = {
      value match {
        case "GET" => GET()
        case "POST" => POST()
        case "PUT" => PUT()
        case "DELETE" => DELETE()
        case "PATCH" => PATCH()
      }
    }

    /**
     * GET and POST http methods.
     */
    sealed abstract class Method(hasBdy: Boolean) {

      /**
       * Check if this HTTP method has/needs a request body
       * @return if body needed
       */
      def hasBody: Boolean = hasBdy

      def name: String

    }

    case class GET(hasBdy: Boolean = false) extends Method(hasBdy) {

      override def name = "GET"
    }

    case class POST(hasBdy: Boolean = true) extends Method(hasBdy) {

      override def name = "POST"
    }

    case class PUT(hasBdy: Boolean = true) extends Method(hasBdy) {

      override def name = "PUT"
    }

    case class DELETE(hasBdy: Boolean = false) extends Method(hasBdy) {

      override def name = "DELETE"
    }

    case class PATCH(hasBdy: Boolean = true) extends Method(hasBdy) {

      override def name = "PATCH"
    }

  }

  /**
   * Common methods for Requests and Responses
   * @param T Type of Base, either Request or Response
   */
  trait Base[T <: Base[T]] {

    /**
     * Get the URL
     * @return URL
     */
    def url: URL

    /**
     * Set the URL
     * @param url new URL
     * @return this, for chaining
     */
    def url(url: URL): T

    /**
     * Get the request method
     * @return method
     */
    def method: Connection.Method.Method

    /**
     * Set the request method
     * @param method new method
     * @return this, for chaining
     */
    def method(method: Connection.Method.Method): T

    /**
     * Get the value of a header. This is a simplified header model, where a header may only have one value.
     * <p/>
     * Header names are case insensitive.
     * @param name name of header (case insensitive)
     * @return value of header, or null if not set.
     * @see #hasHeader(String)
     * @see #cookie(String)
     */
    def header(name: String): String

    /**
     * Set a header. This method will overwrite any existing header with the same case insensitive name.
     * @param name Name of header
     * @param value Value of header
     * @return this, for chaining
     */
    def header(name: String, value: String): T

    /**
     * Check if a header is present
     * @param name name of header (case insensitive)
     * @return if the header is present in this request/response
     */
    def hasHeader(name: String): Boolean

    /**
     * Check if a header is present, with the given value
     * @param name header name (case insensitive)
     * @param value value (case insensitive)
     * @return if the header and value pair are set in this req/res
     */
    def hasHeaderWithValue(name: String, value: String): Boolean

    /**
     * Remove a header by name
     * @param name name of header to remove (case insensitive)
     * @return this, for chaining
     */
    def removeHeader(name: String): T

    /**
     * Retrieve all of the request/response headers as a map
     * @return headers
     */
    def headers: Map[String, String]

    /**
     * Get a cookie value by name from this request/response.
     * <p/>
     * Response objects have a simplified cookie model. Each cookie set in the response is added to the response
     * object's cookie key=value map. The cookie's path, domain, and expiry date are ignored.
     * @param name name of cookie to retrieve.
     * @return value of cookie, or null if not set
     */
    def cookie(name: String): String

    /**
     * Set a cookie in this request/response.
     * @param name name of cookie
     * @param value value of cookie
     * @return this, for chaining
     */
    def cookie(name: String, value: String): T

    /**
     * Check if a cookie is present
     * @param name name of cookie
     * @return if the cookie is present in this request/response
     */
    def hasCookie(name: String): Boolean

    /**
     * Remove a cookie by name
     * @param name name of cookie to remove
     * @return this, for chaining
     */
    def removeCookie(name: String): T

    /**
     * Retrieve all of the request/response cookies as a map
     * @return cookies
     */
    def cookies: Map[String, String]
  }

  /**
   * Represents a HTTP request.
   */
  trait Request extends Base[Connection.Request] {

    /**
     * Get the request timeout, in milliseconds.
     * @return the timeout in milliseconds.
     */
    def timeout: Int

    /**
     * Update the request timeout.
     * @param millis timeout, in milliseconds
     * @return this Request, for chaining
     */
    def timeout(millis: Int): Connection.Request

    /**
     * Get the maximum body size, in bytes.
     * @return the maximum body size, in bytes.
     */
    def maxBodySize: Int

    /**
     * Update the maximum body size, in bytes.
     * @param bytes maximum body size, in bytes.
     * @return this Request, for chaining
     */
    def maxBodySize(bytes: Int): Connection.Request

    /**
     * Get the current followRedirects configuration.
     * @return true if followRedirects is enabled.
     */
    def followRedirects: Boolean

    /**
     * Configures the request to (not) follow server redirects. By default this is <b>true</b>.
     * @param followRedirects true if server redirects should be followed.
     * @return this Request, for chaining
     */
    def followRedirects(followRedirects: Boolean): Connection.Request

    /**
     * Get the current ignoreHttpErrors configuration.
     * @return true if errors will be ignored; false (default) if HTTP errors will cause an IOException to be
     *         thrown.
     */
    def ignoreHttpErrors: Boolean

    /**
     * Configures the request to ignore HTTP errors in the response.
     * @param ignoreHttpErrors set to true to ignore HTTP errors.
     * @return this Request, for chaining
     */
    def ignoreHttpErrors(ignoreHttpErrors: Boolean): Connection.Request

    /**
     * Get the current ignoreContentType configuration.
     * @return true if invalid content-types will be ignored; false (default) if they will cause an IOException to
     *         be thrown.
     */
    def ignoreContentType: Boolean

    /**
     * Configures the request to ignore the Content-Type of the response.
     * @param ignoreContentType set to true to ignore the content type.
     * @return this Request, for chaining
     */
    def ignoreContentType(ignoreContentType: Boolean): Connection.Request

    /**
     * Get the current state of TLS (SSL) certificate validation.
     * @return true if TLS cert validation enabled
     */
    def validateTLSCertificates: Boolean

    /**
     * Set TLS certificate validation.
     * @param value set false to ignore TLS (SSL) certificates
     */
    def validateTLSCertificates(value: Boolean)

    /**
     * Add a data parameter to the request
     * @param keyval data to add.
     * @return this Request, for chaining
     */
    def data(keyval: Connection.KeyVal): Connection.Request

    /**
     * Get all of the request's data parameters
     * @return collection of keyvals
     */
    def data: mutable.Buffer[Connection.KeyVal]

    /**
     * Specify the parser to use when parsing the document.
     * @param parser parser to use.
     * @return this Request, for chaining
     */
    def parser(parser: Parser): Connection.Request

    /**
     * Get the current parser to use when parsing the document.
     * @return current Parser
     */
    def parser: Parser
  }

  /**
   * Represents a HTTP response.
   */
  trait Response extends Base[Connection.Response] {

    /**
     * Get the status code of the response.
     * @return status code
     */
    def statusCode: Int

    /**
     * Get the status message of the response.
     * @return status message
     */
    def statusMessage: String

    /**
     * Get the character set name of the response.
     * @return character set name
     */
    def charset: String

    /**
     * Get the response content type (e.g. "text/html");
     * @return the response content type
     */
    def contentType: String

    /**
     * Parse the body of the response as a Document.
     * @return a parsed Document
     * @throws IOException on error
     */
    @throws(classOf[IOException])
    def parse: Document

    /**
     * Get the body of the response as a plain string.
     * @return body
     */
    def body: String

    /**
     * Get the body of the response as an array of bytes.
     * @return body bytes
     */
    def bodyAsBytes: Array[Byte]
  }

  /**
   * A Key Value tuple.
   */
  trait KeyVal {

    /**
     * Update the key of a keyval
     * @param key new key
     * @return this KeyVal, for chaining
     */
    def key(key: String): Connection.KeyVal

    /**
     * Get the key of a keyval
     * @return the key
     */
    def key: String

    /**
     * Update the value of a keyval
     * @param value the new value
     * @return this KeyVal, for chaining
     */
    def value(value: String): Connection.KeyVal

    /**
     * Get the value of a keyval
     * @return the value
     */
    def value: String

    /**
     * Add or update an input stream to this keyVal
     * @param inputStream new input stream
     * @return this KeyVal, for chaining
     */
    def inputStream(inputStream: InputStream): Connection.KeyVal

    /**
     * Get the input stream associated with this keyval, if any
     * @return input stream if set, or null
     */
    def inputStream: InputStream

    /**
     * Does this keyval have an input stream?
     * @return true if this keyval does indeed have an input stream
     */
    def hasInputStream: Boolean
  }

}