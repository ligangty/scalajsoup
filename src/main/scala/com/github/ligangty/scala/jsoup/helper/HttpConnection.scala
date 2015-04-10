package com.github.ligangty.scala.jsoup.helper

import java.io._
import java.net._
import java.nio.ByteBuffer
import java.nio.charset.Charset
import java.security.{KeyManagementException, NoSuchAlgorithmException, SecureRandom}
import java.security.cert.X509Certificate
import java.util.zip.GZIPInputStream
import javax.net.ssl._

import com.github.ligangty.scala.jsoup.{UnsupportedMimeTypeException, HttpStatusException, Connection}
import Validator._
import com.github.ligangty.scala.jsoup.helper.HttpConnection.KeyVal
import com.github.ligangty.scala.jsoup.nodes.Document
import com.github.ligangty.scala.jsoup.parser.{TokenQueue, Parser}

import scala.collection.mutable
import scala.util.matching.Regex

class HttpConnection private(n: Unit) extends Connection {

  private var req: Connection.Request = null
  private var res: Connection.Response = null

  private def this() {
    this(())
    req = new HttpConnection.Request
    res = new HttpConnection.Response
  }

  def url(url: URL): Connection = {
    req.url(url)
    this
  }

  def url(url: String): Connection = {
    Validator.notEmpty(url, "Must supply a valid URL")
    try {
      req.url(new URL(HttpConnection.encodeUrl(url)))
    } catch {
      case e: MalformedURLException =>
        throw new IllegalArgumentException("Malformed URL: " + url, e)
    }
    this
  }

  def userAgent(userAgent: String): Connection = {
    Validator.notNull(userAgent, "User agent must not be null")
    req.header("User-Agent", userAgent)
    this
  }

  def timeout(millis: Int): Connection = {
    req.timeout(millis)
    this
  }

  def maxBodySize(bytes: Int): Connection = {
    req.maxBodySize(bytes)
    this
  }

  def followRedirects(followRedirects: Boolean): Connection = {
    req.followRedirects(followRedirects)
    this
  }

  def referrer(referrer: String): Connection = {
    Validator.notNull(referrer, "Referrer must not be null")
    req.header("Referer", referrer)
    this
  }

  def method(method: Connection.Method.Method): Connection = {
    req.method(method)
    this
  }

  def ignoreHttpErrors(ignoreHttpErrors: Boolean): Connection = {
    req.ignoreHttpErrors(ignoreHttpErrors)
    this
  }

  def ignoreContentType(ignoreContentType: Boolean): Connection = {
    req.ignoreContentType(ignoreContentType)
    this
  }

  def validateTLSCertificates(value: Boolean): Connection = {
    req.validateTLSCertificates(value)
    this
  }

  def data(key: String, value: String): Connection = {
    req.data(KeyVal.create(key, value))
    this
  }

  def data(key: String, filename: String, inputStream: InputStream): Connection = {
    req.data(KeyVal.create(key, filename, inputStream))
    this
  }

  def data(data: Map[String, String]): Connection = {
    Validator.notNull(data, "Data map must not be null")
    import scala.collection.JavaConversions._
    for (entry <- data.entrySet) {
      req.data(KeyVal.create(entry.getKey, entry.getValue))
    }
    this
  }

  def data(keyvals: String*): Connection = {
    Validator.notNull(keyvals, "Data key value pairs must not be null")
    Validator.isTrue(keyvals.length % 2 == 0, "Must supply an even number of key value pairs")
    for (i <- 0.to(keyvals.length - 1, 2)) {
      val key: String = keyvals(i)
      val value: String = keyvals(i + 1)
      Validator.notEmpty(key, "Data key must not be empty")
      Validator.notNull(value, "Data value must not be null")
      req.data(KeyVal.create(key, value))
    }
    this
  }

  def data(data: Traversable[Connection.KeyVal]): Connection = {
    Validator.notNull(data, "Data collection must not be null")
    for (entry <- data) {
      req.data(entry)
    }
    this
  }

  def header(name: String, value: String): Connection = {
    req.header(name, value)
    this
  }

  def cookie(name: String, value: String): Connection = {
    req.cookie(name, value)
    this
  }

  def cookies(cookies: Map[String, String]): Connection = {
    Validator.notNull(cookies, "Cookie map must not be null")
    for (entry <- cookies) {
      req.cookie(entry._1, entry._2)
    }
    this
  }

  def parser(parser: Parser): Connection = {
    req.parser(parser)
    this
  }

  @throws(classOf[IOException])
  def get: Document = {
    req.method(Connection.Method.GET())
    execute
    res.parse
  }

  @throws(classOf[IOException])
  def post: Document = {
    req.method(Connection.Method.POST())
    execute
    res.parse
  }

  @throws(classOf[IOException])
  def execute: Connection.Response = {
    res = HttpConnection.Response.execute(req)
    res
  }

  def request: Connection.Request = {
    req
  }

  def request(request: Connection.Request): Connection = {
    req = request
    this
  }

  def response: Connection.Response = {
    res
  }

  def response(response: Connection.Response): Connection = {
    res = response
    this
  }
}

object HttpConnection {

  val CONTENT_ENCODING: String = "Content-Encoding"
  private val CONTENT_TYPE: String = "Content-Type"
  private val MULTIPART_FORM_DATA: String = "multipart/form-data"
  private val FORM_URL_ENCODED: String = "application/x-www-form-urlencoded"

  def connect(url: String): Connection = {
    val con: Connection = new HttpConnection
    con.url(url)
    con
  }

  def connect(url: URL): Connection = {
    val con: Connection = new HttpConnection
    con.url(url)
    con
  }

  private def encodeUrl(url: String): String = {
    if (url == null) {
      return null
    }
    url.replaceAll(" ", "%20")
  }

  private def encodeMimeName(value: String): String = {
    if (value == null) {
      return null
    }
    value.replaceAll("\"", "%22")
  }

  private[HttpConnection] abstract class Base[T <: Connection.Base[T]] protected(n: Unit = ()) extends Connection.Base[T] {

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
      if (entry != null) {
        headersMap.remove(entry._1)
      }
      this.asInstanceOf[T]
    }

    def headers: Map[String, String] = headersMap.toMap

    private def getHeaderCaseInsensitive(name: String): String = {
      notNull(name, "Header name must not be null")
      var value: String = headersMap(name)
      if (value == null) {
        value = headersMap(name.toLowerCase)
      }
      if (value == null) {
        val entry = scanHeaders(name)
        if (entry != null) {
          value = entry._2
        }
      }
      value
    }

    private def scanHeaders(name: String): Tuple2[String, String] = {
      val lc: String = name.toLowerCase
      for (entry <- headersMap) {
        if (entry._1.toLowerCase == lc) {
          return entry
        }
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

  class Request private(n: Unit = ()) extends HttpConnection.Base[Connection.Request]() with Connection.Request {

    private var timeoutMilliseconds: Int = 0
    private var maxBodySizeBytes: Int = 0
    private var followRedirectsVal: Boolean = false
    private var dataVal: mutable.Buffer[Connection.KeyVal] = null
    private var ignoreHttpErrorsVal: Boolean = false
    private var ignoreContentTypeVal: Boolean = false
    private var parserVal: Parser = null
    private var ValidatorTSLCertificatesVal: Boolean = true

    private[HttpConnection] def this() {
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
      ValidatorTSLCertificatesVal
    }

    def validateTLSCertificates(value: Boolean) {
      ValidatorTSLCertificatesVal = value
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

    override def data: mutable.Buffer[Connection.KeyVal] = {
      dataVal.toBuffer
    }

    def parser(parser: Parser): HttpConnection.Request = {
      this.parserVal = parser
      this
    }

    def parser: Parser = {
      parserVal
    }
  }

  object Response {

    private[Response] val MAX_REDIRECTS: Int = 20
    private[Response] var sslSocketFactory: SSLSocketFactory = null
    private[Response] val LOCATION: String = "Location"
    /*
     * For example {{{application/atom+xml;charsetVal=utf-8}}}.
     * Stepping through it: start with <code>"application/"</code>, follow with word
     * characters up to a <code>"+xml"</code>, and then maybe more (<code>.*</code>).
     */
    private[Response] val xmlContentTypeRxp: Regex = """application/\w+\+xml.*""".r

    @throws(classOf[IOException])
    private[helper] def execute(req: Connection.Request): HttpConnection.Response = {
      execute(req, null)
    }

    @throws(classOf[IOException])
    private[helper] def execute(req: Connection.Request, previousResponse: HttpConnection.Response): HttpConnection.Response = {
      Validator.notNull(req, "Request must not be null")
      val protocol: String = req.url.getProtocol
      if (!(protocol == "http") && !(protocol == "https")) {
        throw new MalformedURLException("Only http & https protocols supported")
      }
      // set up the request for execution
      var mimeBoundary: String = null
      if (!req.method.hasBody && req.data.size > 0) {
        serialiseRequestUrl(req) // appends query string
      }
      else if (req.method.hasBody) {
        mimeBoundary = setOutputContentType(req)
      }
      val conn: HttpURLConnection = createConnection(req)
      var res: HttpConnection.Response = null
      try {
        conn.connect()
        if (conn.getDoOutput) {
          writePost(req, conn.getOutputStream, mimeBoundary)
        }
        val status: Int = conn.getResponseCode
        res = new HttpConnection.Response(previousResponse)
        res.setupFromConnection(conn, previousResponse)
        res.req = req
        // redirect if there's a location header (from 3xx, or 201 etc)
        if (res.hasHeader(LOCATION) && req.followRedirects) {
          // always redirect with a get. any data param from original req are dropped.
          req.method(Connection.Method.GET())
          req.data.clear()
          var location: String = res.header(LOCATION)
          // fix broken Location: http:/temp/AAG_New/en/index.php
          if (location != null && location.startsWith("http:/") && location.charAt(6) != '/') {
            location = location.substring(6)
          }
          req.url(new URL(req.url, encodeUrl(location)))
          // add response cookies to request (for e.g. login posts)
          for (cookie <- res.cookies) {
            req.cookie(cookie._1, cookie._2)
          }
          return execute(req, res)
        }
        if ((status < 200 || status >= 400) && !req.ignoreHttpErrors) {
          throw new HttpStatusException("HTTP error fetching URL", status, req.url.toString)
        }
        // check that we can handle the returned content type; if not, abort before fetching it
        val contentType: String = res.contentTypeVal
        if (contentType != null &&
                !req.ignoreContentType &&
                !contentType.startsWith("text/") &&
                !contentType.startsWith("application/xml") &&
                xmlContentTypeRxp.findFirstIn(contentType).isEmpty) {
          throw new UnsupportedMimeTypeException("Unhandled content type. Must be text/*, application/xml, or application/xhtml+xml", contentType, req.url.toString)
        }
        var bodyStream: InputStream = null
        var dataStream: InputStream = null
        try {
          dataStream = if (conn.getErrorStream != null) {
            conn.getErrorStream
          } else {
            conn.getInputStream
          }
          bodyStream = if (res.hasHeaderWithValue(CONTENT_ENCODING, "gzip")) {
            new BufferedInputStream(new GZIPInputStream(dataStream))
          } else {
            new BufferedInputStream(dataStream)
          }
          res.byteData = DataUtil.readToByteBuffer(bodyStream, req.maxBodySize)
          res.charsetVal = DataUtil.getCharsetFromContentType(res.contentTypeVal) // may be null, readInputStream deals with it
        } finally {
          if (bodyStream != null) {
            bodyStream.close()
          }
          if (dataStream != null) {
            dataStream.close()
          }
        }
      } finally {
        // per Java's documentation, this is not necessary, and precludes keepalives. However in practise,
        // connection errors will not be released quickly enough and can cause a too many open files error.
        conn.disconnect()
      }
      res.executed = true
      res
    }

    // set up connection defaults, and details from request
    @throws(classOf[IOException])
    private[Response] def createConnection(req: Connection.Request): HttpURLConnection = {
      val conn: HttpURLConnection = req.url.openConnection.asInstanceOf[HttpURLConnection]
      conn.setRequestMethod(req.method.name)
      conn.setInstanceFollowRedirects(false) // don't rely on native redirection support
      conn.setConnectTimeout(req.timeout)
      conn.setReadTimeout(req.timeout)
      conn match {
        case con: HttpsURLConnection =>
          if (!req.validateTLSCertificates) {
            initUnSecureTSL()
            con.setSSLSocketFactory(sslSocketFactory)
            con.setHostnameVerifier(getInsecureVerifier)
          }
        case _ =>
      }
      if (req.method.hasBody) {
        conn.setDoOutput(true)
      }
      if (req.cookies.size > 0) {
        conn.addRequestProperty("Cookie", getRequestCookieString(req))
      }
      for (header <- req.headers) {
        conn.addRequestProperty(header._1, header._2)
      }
      conn
    }

    /**
     * Instantiate Hostname Verifier that does nothing.
     * This is used for connections with disabled SSL certificates validation.
     *
     * @return Hostname Verifier that does nothing and accepts all hostnames
     */
    private[Response] def getInsecureVerifier: HostnameVerifier = {
      new HostnameVerifier {
        def verify(urlHostName: String, session: SSLSession): Boolean = {
          true
        }
      }
    }

    /**
     * Initialise Trust manager that does not validate certificate chains and
     * add it to current SSLContext.
     * <p/>
     * please not that this method will only perform action if sslSocketFactory is not yet
     * instantiated.
     *
     * @throws IOException
     */
    @throws(classOf[IOException])
    private[Response] def initUnSecureTSL(): Unit = {
      if (sslSocketFactory == null) {
        // Create a trust manager that does not validate certificate chains
        val trustAllCerts: Array[TrustManager] = Array[TrustManager](new X509TrustManager {
          def checkClientTrusted(chain: Array[X509Certificate], authType: String) {}

          def checkServerTrusted(chain: Array[X509Certificate], authType: String) {}

          def getAcceptedIssuers: Array[X509Certificate] = {
            null
          }
        })
        // Install the all-trusting trust manager
        var sslContext: SSLContext = null
        try {
          sslContext = SSLContext.getInstance("SSL")
          sslContext.init(null, trustAllCerts, new SecureRandom())
          // Create an ssl socket factory with our all-trusting manager
          sslSocketFactory = sslContext.getSocketFactory
        }
        catch {
          case e: NoSuchAlgorithmException =>
            throw new IOException("Can't create unsecure trust manager")
          case e: KeyManagementException =>
            throw new IOException("Can't create unsecure trust manager")
        }
      }
    }

    private def setOutputContentType(req: Connection.Request): String = {
      // multipart mode, for files. add the header if we see something with an inputstream, and return a non-null boundary
      var needsMulti: Boolean = false
      import scala.util.control.Breaks._
      breakable {
        for (keyVal <- req.data) {
          if (keyVal.hasInputStream) {
            needsMulti = true
            break()
          }
        }
      }
      var bound: String = null
      if (needsMulti) {
        bound = DataUtil.mimeBoundary
        req.header(CONTENT_TYPE, MULTIPART_FORM_DATA + "; boundary=" + bound)
      } else {
        req.header(CONTENT_TYPE, FORM_URL_ENCODED)
      }
      bound
    }

    @throws(classOf[IOException])
    private def writePost(req: Connection.Request, outputStream: OutputStream, bound: String) {
      val data: mutable.Buffer[Connection.KeyVal] = req.data
      val w: BufferedWriter = new BufferedWriter(new OutputStreamWriter(outputStream, DataUtil.defaultCharset))
      if (bound != null) {
        // boundary will be set if we're in multipart mode
        for (keyVal <- data) {
          w.write("--")
          w.write(bound)
          w.write("\r\n")
          w.write("Content-Disposition: form-data; name=\"")
          w.write(encodeMimeName(keyVal.key)) // encodes " to %22
          w.write("\"")
          if (keyVal.hasInputStream) {
            w.write("; filename=\"")
            w.write(encodeMimeName(keyVal.value))
            w.write("\"\r\nContent-Type: application/octet-stream\r\n\r\n")
            w.flush()
            DataUtil.crossStreams(keyVal.inputStream, outputStream)
            outputStream.flush()
          } else {
            w.write("\r\n\r\n")
            w.write(keyVal.value)
          }
          w.write("\r\n")
        }
        w.write("--")
        w.write(bound)
        w.write("--")
      } else {
        // regular form data (application/x-www-form-urlencoded)
        var first: Boolean = true
        for (keyVal <- data) {
          if (!first) {
            w.append('&')
          } else {
            first = false
          }
          w.write(URLEncoder.encode(keyVal.key, DataUtil.defaultCharset))
          w.write('=')
          w.write(URLEncoder.encode(keyVal.value, DataUtil.defaultCharset))
        }
      }
      w.close()
    }

    private def getRequestCookieString(req: Connection.Request): String = {
      val sb: StringBuilder = new StringBuilder
      var first: Boolean = true
      for (cookie <- req.cookies) {
        if (!first) {
          sb.append("; ")
        } else {
          first = false
        }
        sb.append(cookie._1).append('=').append(cookie._2)
        // todo: spec says only ascii, no escaping / encoding defined. validate on set? or escape somehow here?
      }
      sb.toString()
    }

    @throws(classOf[IOException])
    private def serialiseRequestUrl(req: Connection.Request) {
      val in: URL = req.url
      val url: StringBuilder = new StringBuilder
      var first: Boolean = true
      // reconstitute the query, ready for appends
      url.append(in.getProtocol).append("://").append(in.getAuthority).append(in.getPath).append("?")
      if (in.getQuery != null) {
        url.append(in.getQuery)
        first = false
      }
      for (keyVal <- req.data) {
        if (!first) {
          url.append('&')
        } else {
          first = false
        }
        url.append(URLEncoder.encode(keyVal.key, DataUtil.defaultCharset)).append('=').append(URLEncoder.encode(keyVal.value, DataUtil.defaultCharset))
      }
      req.url(new URL(url.toString()))
      req.data.clear()
    }
  }

  class Response private(n: Unit = ()) extends HttpConnection.Base[Connection.Response] with Connection.Response {

    private var statusCodeVal: Int = 0
    private var statusMessageVal: String = null
    private var byteData: ByteBuffer = null
    private var charsetVal: String = null
    private var contentTypeVal: String = null
    private var executed: Boolean = false
    private var numRedirects: Int = 0
    private var req: Connection.Request = null

    private[helper] def this() {
      this(())
    }

    @throws(classOf[IOException])
    private def this(previousResponse: HttpConnection.Response) {
      this(())
      if (previousResponse != null) {
        numRedirects = previousResponse.numRedirects + 1
        if (numRedirects >= Response.MAX_REDIRECTS) {
          throw new IOException("Too many redirects occurred trying to load URL %s".format(previousResponse.url))
        }
      }
    }

    def statusCode: Int = {
      statusCodeVal
    }

    def statusMessage: String = {
      statusMessageVal
    }

    def charset: String = {
      charsetVal
    }

    def contentType: String = {
      contentTypeVal
    }

    @throws(classOf[IOException])
    def parse: Document = {
      Validator.isTrue(executed, "Request must be executed (with .execute(), .get(), or .post() before parsing response")
      val doc: Document = DataUtil.parseByteData(byteData, charsetVal, url.toExternalForm, req.parser)
      byteData.rewind
      charsetVal = doc.outputSettings.charset.name // update charset from meta-equiv, possibly
      doc
    }

    def body: String = {
      Validator.isTrue(executed, "Request must be executed (with .execute(), .get(), or .post() before getting response body")
      // charset gets set from header on execute, and from meta-equiv on parse. parse may not have happened yet
      var body: String = null
      if (charsetVal == null) {
        body = Charset.forName(DataUtil.defaultCharset).decode(byteData).toString
      }
      else {
        body = Charset.forName(charset).decode(byteData).toString
      }
      byteData.rewind
      body
    }

    def bodyAsBytes: Array[Byte] = {
      Validator.isTrue(executed, "Request must be executed (with .execute(), .get(), or .post() before getting response body")
      byteData.array
    }

    @throws(classOf[IOException])
    private def setupFromConnection(conn: HttpURLConnection, previousResponse: Connection.Response) {
      methodVal = Connection.Method.valueOf(conn.getRequestMethod)
      urlVal = conn.getURL
      statusCodeVal = conn.getResponseCode
      statusMessageVal = conn.getResponseMessage
      contentTypeVal = conn.getContentType
      import scala.collection.JavaConversions._
      val resHeaders = conn.getHeaderFields.toMap
      processResponseHeaders(resHeaders)
      // if from a redirect, map previous response cookies into this response
      if (previousResponse != null) {
        for (prevCookie <- previousResponse.cookies) {
          if (!hasCookie(prevCookie._1)) {
            cookie(prevCookie._1, prevCookie._2)
          }
        }
      }
    }

    private[helper] def processResponseHeaders(resHeaders: Map[String, java.util.List[String]]) {
      import scala.util.control.Breaks

      for (entry <- resHeaders) {
        val continue = new Breaks
        continue.breakable {
          val name: String = entry._1
          if (name == null) {
            continue.break() // http/1.1 line
          }
          import scala.collection.JavaConversions._
          val values: List[String] = entry._2.toList
          if (name.equalsIgnoreCase("Set-Cookie")) {
            for (value <- values) {
              val inContinue = new Breaks
              inContinue.breakable {
                if (value == null) {
                  inContinue.break()
                }
                val cd: TokenQueue = new TokenQueue(value)
                val cookieName: String = cd.chompTo("=").trim
                var cookieVal: String = cd.consumeTo(";").trim
                if (cookieVal == null) {
                  cookieVal = ""
                }
                // ignores path, date, domain, validateTLSCertificates et al. req'd?
                // name not blank, value not null
                if (cookieName != null && cookieName.length > 0) {
                  cookie(cookieName, cookieVal)
                }
              }
            }
          } else {
            // only take the first instance of each header
            if (values.nonEmpty) {
              header(name, values.head)
            }
          }
        }
      }
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
