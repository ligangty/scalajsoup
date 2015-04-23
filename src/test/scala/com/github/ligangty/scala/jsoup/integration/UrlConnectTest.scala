package com.github.ligangty.scala.jsoup.integration

import java.io.{IOException, FileInputStream, File}
import java.net.{MalformedURLException, URL}

import com.github.ligangty.scala.jsoup.{HttpStatusException, UnsupportedMimeTypeException, Connection, Jsoup}
import com.github.ligangty.scala.jsoup.helper.W3CDom
import com.github.ligangty.scala.jsoup.nodes.{FormElement, Document}
import org.scalatest.{FunSuite, Ignore}

/**
 * Tests the URL connection. Not enabled by default, so tests don't require network connection.
 */
//@Ignore // ignored by default so tests don't require network access. comment out to enable.
class UrlConnectTest extends FunSuite {

  private val WEBSITE_WITH_INVALID_CERTIFICATE: String = "https://certs.cac.washington.edu/CAtest/"
  private val WEBSITE_WITH_SNI: String = "https://jsoup.org/"
  private val echoURL: String = "http://direct.infohound.net/tools/q.pl"

  test("fetchURl") {
    val url: String = "http://jsoup.org"
    val doc: Document = Jsoup.parse(new URL(url), 10 * 1000)
    assert(doc.title.contains("jsoup"))
  }

  test("fetchURIWithWihtespace") {
    val con: Connection = Jsoup.connect("http://try.jsoup.org/#with whitespaces")
    val doc: Document = con.get
    assert(doc.title.contains("jsoup"))
  }

  test("fetchBaidu") {
    val res: Connection.Response = Jsoup.connect("http://www.baidu.com/").timeout(10 * 1000).execute
    val doc: Document = res.parse
    assert("UTF-8" == doc.outputSettings.charset.displayName)
    assert("UTF-8" == res.charset)
    assert(res.hasCookie("BAIDUID"))
    assert("text/html; charset=utf-8" == res.contentType)
  }

  test("exceptOnUnknownContentType") {
    val url: String = "http://direct.jsoup.org/rez/osi_logo.png"
    var threw: Boolean = false
    try {
      Jsoup.parse(new URL(url), 3000)
    } catch {
      case e: UnsupportedMimeTypeException =>
        threw = true
        assert("com.github.ligangty.scala.jsoup.UnsupportedMimeTypeException: Unhandled content type. Must be text/*, application/xml, or application/xhtml+xml. Mimetype=image/png, URL=http://direct.jsoup.org/rez/osi_logo.png" == e.toString)
        assert(url == e.getUrl)
        assert("image/png" == e.getMimeType)
      case e: IOException =>
        assert(threw)
    }
  }

  test("exceptOnUnsupportedProtocol") {
    val url: String = "file://etc/passwd"
    var threw: Boolean = false
    try {
      Jsoup.connect(url).get
    } catch {
      case e: MalformedURLException =>
        threw = true
        assert("java.net.MalformedURLException: Only http & https protocols supported" == e.toString)
      case e: IOException =>
    }
    assert(threw)
  }


  test("ignoresContentTypeIfSoConfigured") {
    val doc: Document = Jsoup.connect("http://jsoup.org/rez/osi_logo.png").ignoreContentType(true).get
    assert("" == doc.title)
  }


  test("doesPost") {
    val doc: Document = Jsoup.connect(echoURL).data("uname", "Jsoup", "uname", "Jonathan", "百", "度一下").cookie("auth", "token").post
    assert("POST" == ihVal("REQUEST_METHOD", doc))
    assert("auth=token" == ihVal("HTTP_COOKIE", doc))
    assert("度一下" == ihVal("百", doc))
    assert("Jsoup, Jonathan" == ihVal("uname", doc))
  }


  test("doesGet") {
    val con: Connection = Jsoup.connect(echoURL + "?what=the").userAgent("Mozilla").referrer("http://example.com").data("what", "about & me?")
    val doc: Document = con.get
    assert("what=the&what=about+%26+me%3F" == ihVal("QUERY_STRING", doc))
    assert("the, about & me?" == ihVal("what", doc))
    assert("Mozilla" == ihVal("HTTP_USER_AGENT", doc))
    assert("http://example.com" == ihVal("HTTP_REFERER", doc))
  }


  test("doesPut") {
    val res: Connection.Response = Jsoup.connect(echoURL).data("uname", "Jsoup", "uname", "Jonathan", "百", "度一下").cookie("auth", "token").method(Connection.Method.PUT()).execute
    val doc: Document = res.parse
    assert("PUT" == ihVal("REQUEST_METHOD", doc))
    assert("auth=token" == ihVal("HTTP_COOKIE", doc))
  }



  test("followsTempRedirect") {
    val con: Connection = Jsoup.connect("http://direct.infohound.net/tools/302.pl")
    val doc: Document = con.get
    assert(doc.title.contains("jsoup"))
  }


  test("followsNewTempRedirect") {
    val con: Connection = Jsoup.connect("http://direct.infohound.net/tools/307.pl")
    val doc: Document = con.get
    assert(doc.title.contains("jsoup"))
    assert("http://jsoup.org" == con.response.url.toString)
  }


  test("postRedirectsFetchWithGet") {
    val con: Connection = Jsoup.connect("http://direct.infohound.net/tools/302.pl").data("Argument", "Riposte").method(Connection.Method.POST())
    val res: Connection.Response = con.execute
    assert("http://jsoup.org" == res.url.toExternalForm)
    assert(Connection.Method.GET() == res.method)
  }


  test("followsRedirectToHttps") {
    val con: Connection = Jsoup.connect("http://direct.infohound.net/tools/302-secure.pl")
    con.data("id", "5")
    val doc: Document = con.get
    assert(doc.title.contains("Google"))
  }


  test("followsRelativeRedirect") {
    val con: Connection = Jsoup.connect("http://direct.infohound.net/tools/302-rel.pl")
    val doc: Document = con.post
    assert(doc.title.contains("HTML Tidy Online"))
  }


  test("followsRedirectsWithWithespaces") {
    val con: Connection = Jsoup.connect("http://tinyurl.com/kgofxl8")
    val doc: Document = con.get
    assert(doc.title.contains("Google"))
  }


  test("gracefullyHandleBrokenLocationRedirect") {
    val con: Connection = Jsoup.connect("http://aag-ye.com")
    con.get
    assert(true)
  }

  test("throwsExceptionOnError") {
    val url: String = "http://direct.infohound.net/tools/404"
    val con: Connection = Jsoup.connect(url)
    var threw: Boolean = false
    try {
      con.get
    } catch {
      case e: HttpStatusException =>
        threw = true
        assert("com.github.ligangty.scala.jsoup.HttpStatusException: HTTP error fetching URL. Status=404, URL=http://direct.infohound.net/tools/404" == e.toString)
        assert(url == e.getUrl)
        assert(404 == e.getStatusCode)
      case e: IOException =>
    }
    assert(threw)
  }


  test("ignoresExceptionIfSoConfigured") {
    val con: Connection = Jsoup.connect("http://direct.infohound.net/tools/404").ignoreHttpErrors(true)
    val res: Connection.Response = con.execute
    val doc: Document = res.parse
    assert(404 == res.statusCode)
    assert("404 Not Found" == doc.select("h1").first().text())
  }


  test("ignores500tExceptionIfSoConfigured") {
    val con: Connection = Jsoup.connect("http://direct.infohound.net/tools/500.pl").ignoreHttpErrors(true)
    val res: Connection.Response = con.execute
    val doc: Document = res.parse
    assert(500 == res.statusCode)
    assert("Application Error" == res.statusMessage)
    assert("Woops" == doc.select("h1").first().text())
  }


  test("ignores500NoWithContentExceptionIfSoConfigured") {
    val con: Connection = Jsoup.connect("http://direct.infohound.net/tools/500-no-content.pl").ignoreHttpErrors(true)
    val res: Connection.Response = con.execute
    res.parse
    assert(500 == res.statusCode)
    assert("Application Error" == res.statusMessage)
  }


  test("ignores200NoWithContentExceptionIfSoConfigured") {
    val con: Connection = Jsoup.connect("http://direct.infohound.net/tools/200-no-content.pl").ignoreHttpErrors(true)
    val res: Connection.Response = con.execute
    res.parse
    assert(200 == res.statusCode)
    assert("All Good" == res.statusMessage)
  }


  test("doesntRedirectIfSoConfigured") {
    val con: Connection = Jsoup.connect("http://direct.infohound.net/tools/302.pl").followRedirects(false)
    val res: Connection.Response = con.execute
    assert(302 == res.statusCode)
    assert("http://jsoup.org" == res.header("Location"))
  }


  test("redirectsResponseCookieToNextResponse") {
    val con: Connection = Jsoup.connect("http://direct.infohound.net/tools/302-cookie.pl")
    val res: Connection.Response = con.execute
    assert("asdfg123" == res.cookie("token"))
    val doc: Document = res.parse
    assert("uid=jhy; token=asdfg123" == ihVal("HTTP_COOKIE", doc))
  }

  test("maximumRedirects") {
    var threw: Boolean = false
    try {
      Jsoup.connect("http://direct.infohound.net/tools/loop.pl").get
    } catch {
      case e: IOException =>
        assert(e.getMessage.contains("Too many redirects"))
        threw = true
    }
    assert(threw)
  }


  test("multiCookieSet") {
    val con: Connection = Jsoup.connect("http://direct.infohound.net/tools/302-cookie.pl")
    val res: Connection.Response = con.execute
    val cookies: Map[String, String] = res.cookies
    assert("asdfg123" == cookies("token"))
    assert("jhy" == cookies("uid"))
    val doc: Document = Jsoup.connect(echoURL).cookies(cookies).get
    assert("uid=jhy; token=asdfg123" == ihVal("HTTP_COOKIE", doc))
  }


  test("handlesDodgyCharset") {
    val url: String = "http://direct.infohound.net/tools/bad-charset.pl"
    val res: Connection.Response = Jsoup.connect(url).execute
    assert("text/html; charset=UFT8" == res.header("Content-Type"))
    assert(null == res.charset)
    val doc: Document = res.parse
    assert(doc.text().contains("Hello!"))
    assert("UTF-8" == res.charset)
  }


  test("maxBodySize") {
    val url: String = "http://direct.infohound.net/tools/large.html"
    val defaultRes: Connection.Response = Jsoup.connect(url).execute
    val smallRes: Connection.Response = Jsoup.connect(url).maxBodySize(50 * 1024).execute
    val mediumRes: Connection.Response = Jsoup.connect(url).maxBodySize(200 * 1024).execute
    val largeRes: Connection.Response = Jsoup.connect(url).maxBodySize(300 * 1024).execute
    val unlimitedRes: Connection.Response = Jsoup.connect(url).maxBodySize(0).execute
    val actualString: Int = 280735
    assert(actualString == defaultRes.body.length)
    assert(50 * 1024 == smallRes.body.length)
    assert(200 * 1024 == mediumRes.body.length)
    assert(actualString == largeRes.body.length)
    assert(actualString == unlimitedRes.body.length)
    val actualDocText: Int = 269541
    assert(actualDocText == defaultRes.parse.text().length)
    assert(49165 == smallRes.parse.text().length)
    assert(196577 == mediumRes.parse.text().length)
    assert(actualDocText == largeRes.parse.text().length)
    assert(actualDocText == unlimitedRes.parse.text().length)
  }

  /**
   * Verify that security disabling feature works properly.
   * <p/>
   * 1. try to hit url with invalid certificate and evaluate that exception is thrown
   *
   */
  test("testUnsafeFail") {
    intercept[IOException] {
      val url: String = WEBSITE_WITH_INVALID_CERTIFICATE
      Jsoup.connect(url).execute
    }
  }

  /**
   * Verify that requests to websites with SNI fail on jdk 1.6
   * <p/>
   * read for more details:
   * http://en.wikipedia.org/wiki/Server_Name_Indication
   *
   * Test is ignored independent from others as it requires JDK 1.6
   */
  test("testSNIFail") {
    intercept[IOException] {
      val url: String = WEBSITE_WITH_SNI
      Jsoup.connect(url).execute
    }
  }

  /**
   * Verify that requests to websites with SNI pass
   * <p/>
   * <b>NB!</b> this test is FAILING right now on jdk 1.6
   *
   */
  test("testSNIPass") {
    val url: String = WEBSITE_WITH_SNI
    val defaultRes: Connection.Response = Jsoup.connect(url).validateTLSCertificates(false).execute
    assert(defaultRes.statusCode == 200)
  }

  /**
   * Verify that security disabling feature works properly.
   * <p/>
   * 1. disable security checks and call the same url to verify that content is consumed correctly
   *
   */
  test("testUnsafePass") {
    val url: String = WEBSITE_WITH_INVALID_CERTIFICATE
    val defaultRes: Connection.Response = Jsoup.connect(url).validateTLSCertificates(false).execute
    assert(defaultRes.statusCode == 200)
  }


  test("shouldWorkForCharsetInExtraAttribute") {
    val res: Connection.Response = Jsoup.connect("https://www.creditmutuel.com/groupe/fr/").execute
    res.parse
    assert("ISO-8859-1" == res.charset)
  }

  // The following tests were added to test specific domains if they work. All code paths
  // which make the following test green are tested in other unit or integration tests, so the following lines
  // could be deleted

  test("shouldSelectFirstCharsetOnWeirdMultileCharsetsInMetaTags") {
    val res: Connection.Response = Jsoup.connect("http://aamo.info/").execute
    res.parse
    assert("ISO-8859-1" == res.charset)
  }


  test("shouldParseBrokenHtml5MetaCharsetTagCorrectly") {
    val res: Connection.Response = Jsoup.connect("http://9kuhkep.net").execute
    res.parse
    assert("UTF-8" == res.charset)
  }


  test("shouldEmptyMetaCharsetCorrectly") {
    val res: Connection.Response = Jsoup.connect("http://aastmultimedia.com").execute
    res.parse
    assert("UTF-8" == res.charset)
  }


  test("shouldWorkForDuplicateCharsetInTag") {
    val res: Connection.Response = Jsoup.connect("http://aaptsdassn.org").execute
    res.parse
    assert("ISO-8859-1" == res.charset)
  }


  test("baseHrefCorrectAfterHttpEquiv") {
    val res: Connection.Response = Jsoup.connect("http://direct.infohound.net/tools/charset-base.html").execute
    val doc: Document = res.parse
    assert("http://example.com/foo.jpg" == doc.select("img").first().absUrl("src"))
  }

  /**
   * Test fetching a form, and submitting it with a file attached.
   */

  test("postHtmlFile") {
    val index: Document = Jsoup.connect("http://direct.infohound.net/tidy/").get
    val form: FormElement = index.select("[name=tidy]").forms(0)
    val post: Connection = form.submit
    val uploadFile: File = ParseTest.getFile("/htmltests/google-ipod.html")
    val stream: FileInputStream = new FileInputStream(uploadFile)
    for (keyVal <- post.request.data) {
      if (keyVal.key == "_file") {
        keyVal.value("check.html")
        keyVal.inputStream(stream)
      }
    }
    var res: Connection.Response = null
    try {
      res = post.execute
    } finally {
      stream.close()
    }
    val out: Document = res.parse
    assert(out.text().contains("HTML Tidy Complete"))
  }

  /**
   * Tests upload of binary content to a remote service.
   */

  test("postJpeg") {
    val thumb: File = ParseTest.getFile("/htmltests/thumb.jpg")
    val result: Document = Jsoup.connect("http://regex.info/exif.cgi").data("f", thumb.getName, new FileInputStream(thumb)).post
    assert("Baseline DCT, Huffman coding" == result.select("td:contains(Process) + td").text)
  }


  test("handles201Created") {
    val doc: Document = Jsoup.connect("http://direct.infohound.net/tools/201.pl").get
    assert("http://jsoup.org" == doc.location)
  }


  test("fetchToW3c") {
    val url: String = "http://jsoup.org"
    val doc: Document = Jsoup.connect(url).get
    val dom: W3CDom = new W3CDom
    val wDoc: org.w3c.dom.Document = dom.fromJsoup(doc)
    assert(url == wDoc.getDocumentURI)
    val html: String = dom.asString(wDoc)
    assert(html.contains("jsoup"))
  }

  private def ihVal(key: String, doc: Document): String = {
    doc.select("th:contains(" + key + ") + td").first().text()
  }
}

