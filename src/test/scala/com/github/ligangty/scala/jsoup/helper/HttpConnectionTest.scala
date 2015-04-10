package com.github.ligangty.scala.jsoup.helper

import java.net.URL

import com.github.ligangty.scala.jsoup.Connection
import com.github.ligangty.scala.jsoup.integration.ParseTest
import org.scalatest.FunSuite

import scala.collection.mutable

class HttpConnectionTest extends FunSuite {

  /* most actual network http connection tests are in integration */

  test("throwsExceptionOnParseWithoutExecute") {
    intercept[IllegalArgumentException] {
      val con: Connection = HttpConnection.connect("http://example.com")
      con.response.parse
    }
  }

  test("throwsExceptionOnBodyWithoutExecute") {
    intercept[IllegalArgumentException] {
      val con: Connection = HttpConnection.connect("http://example.com")
      con.response.body
    }
  }

  test("throwsExceptionOnBodyAsBytesWithoutExecute") {
    intercept[IllegalArgumentException] {
      val con: Connection = HttpConnection.connect("http://example.com")
      con.response.bodyAsBytes
    }
  }

  test("caseInsensitiveHeaders") {
    val res: Connection.Response = new HttpConnection.Response
    val headers: mutable.Map[String, String] = mutable.Map(res.headers.toSeq: _*)
    headers.put("Accept-Encoding", "gzip")
    headers.put("content-type", "text/html")
    headers.put("refErrer", "http://example.com")
    assert(res.hasHeader("Accept-Encoding"))
    assert(res.hasHeader("accept-encoding"))
    assert(res.hasHeader("accept-Encoding"))
    assert("gzip" == res.header("accept-Encoding"))
    assert("text/html" == res.header("Content-Type"))
    assert("http://example.com" == res.header("Referrer"))
    res.removeHeader("Content-Type")
    assert(!res.hasHeader("content-type"))
    res.header("accept-encoding", "deflate")
    assert("deflate" == res.header("Accept-Encoding"))
    assert("deflate" == res.header("accept-Encoding"))
  }

  test("ignoresEmptySetCookies") {
    val headers: mutable.Map[String, java.util.List[String]] = new mutable.HashMap[String, java.util.List[String]]
    headers.put("Set-Cookie", java.util.Collections.emptyList())
    val res: HttpConnection.Response = new HttpConnection.Response
    res.processResponseHeaders(headers.toMap)
    assert(0 == res.cookies.size)
  }

  test("ignoresEmptyCookieNameAndVals") {
    val headers: mutable.Map[String, java.util.List[String]] = mutable.HashMap[String, java.util.List[String]]()
    val cookieStrings: java.util.List[String] = new java.util.ArrayList[String]
    cookieStrings.add(null)
    cookieStrings.add("")
    cookieStrings.add("one")
    cookieStrings.add("two=")
    cookieStrings.add("three=;")
    cookieStrings.add("four=data; Domain=.example.com; Path=/")
    headers.put("Set-Cookie", cookieStrings)
    val res: HttpConnection.Response = new HttpConnection.Response
    res.processResponseHeaders(headers.toMap)
    assert(4 == res.cookies.size)
    assert("" == res.cookie("one"))
    assert("" == res.cookie("two"))
    assert("" == res.cookie("three"))
    assert("data" == res.cookie("four"))
  }

  test("connectWithUrl") {
    val con: Connection = HttpConnection.connect(new URL("http://example.com"))
    assert("http://example.com" == con.request.url.toExternalForm)
  }

  test("throwsOnMalformedUrl") {
    intercept[IllegalArgumentException] {
      val con: Connection = HttpConnection.connect("bzzt")
    }
  }

  test("userAgent") {
    val con: Connection = HttpConnection.connect("http://example.com/")
    con.userAgent("Mozilla")
    assert("Mozilla" == con.request.header("User-Agent"))
  }

  test("timeout") {
    val con: Connection = HttpConnection.connect("http://example.com/")
    con.timeout(1000)
    assert(1000 == con.request.timeout)
  }

  test("referrer") {
    val con: Connection = HttpConnection.connect("http://example.com/")
    con.referrer("http://foo.com")
    assert("http://foo.com" == con.request.header("Referer"))
  }

  test("method") {
    val con: Connection = HttpConnection.connect("http://example.com/")
    assert(Connection.Method.GET() == con.request.method)
    con.method(Connection.Method.POST())
    assert(Connection.Method.POST() == con.request.method)
  }

  test("throwsOnOdddData") {
    intercept[IllegalArgumentException] {
      val con: Connection = HttpConnection.connect("http://example.com/")
      con.data("Name", "val", "what")
    }
  }

  test("data") {
    val con: Connection = HttpConnection.connect("http://example.com/")
    con.data("Name", "Val", "Foo", "bar")
    val values: mutable.Buffer[Connection.KeyVal] = con.request.data
    val data: Array[AnyRef] = values.toArray
    val one: Connection.KeyVal = data(0).asInstanceOf[Connection.KeyVal]
    val two: Connection.KeyVal = data(1).asInstanceOf[Connection.KeyVal]
    assert("Name" == one.key)
    assert("Val" == one.value)
    assert("Foo" == two.key)
    assert("bar" == two.value)
  }

  test("cookie") {
    val con: Connection = HttpConnection.connect("http://example.com/")
    con.cookie("Name", "Val")
    assert("Val" == con.request.cookie("Name"))
  }

  test("inputStream") {
    var kv: Connection.KeyVal = HttpConnection.KeyVal.create("file", "thumb.jpg", ParseTest.inputStreamFrom("Check"))
    assert("file" == kv.key)
    assert("thumb.jpg" == kv.value)
    assert(kv.hasInputStream)
    kv = HttpConnection.KeyVal.create("one", "two")
    assert("one" == kv.key)
    assert("two" == kv.value)
    assert(!kv.hasInputStream)
  }
}
