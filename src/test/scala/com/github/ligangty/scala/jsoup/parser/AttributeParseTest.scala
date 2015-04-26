package com.github.ligangty.scala.jsoup.parser

import com.github.ligangty.scala.jsoup.Jsoup
import com.github.ligangty.scala.jsoup.nodes.{Attributes, Element}
import com.github.ligangty.scala.jsoup.select.Elements
import org.scalatest.FunSuite

/**
 * Test suite for attribute parser.
 */
class AttributeParseTest extends FunSuite{

  test("parsesRoughAttributeString") {
    val html: String = "<a id=\"123\" class=\"baz = 'bar'\" style = 'border: 2px'qux zim foo = 12 mux=18 />"
    val el: Element = Jsoup.parse(html).getElementsByTag("a").get(0)
    val attr: Attributes = el.attributes
    assert(7== attr.size)
    assert("123"== attr.get("id"))
    assert("baz = 'bar'"== attr.get("class"))
    assert("border: 2px"== attr.get("style"))
    assert(""== attr.get("qux"))
    assert(""== attr.get("zim"))
    assert("12"== attr.get("foo"))
    assert("18"== attr.get("mux"))
  }

  test("handlesNewLinesAndReturns") {
    val html: String = "<a\r\nfoo='bar\r\nqux'\r\nbar\r\n=\r\ntwo>One</a>"
    val el: Element = Jsoup.parse(html).select("a").first()
    assert(2== el.attributes.size)
    assert("bar\r\nqux"== el.attr("foo"))
    assert("two"== el.attr("bar"))
  }

  test("parsesEmptyString") {
    val html: String = "<a />"
    val el: Element = Jsoup.parse(html).getElementsByTag("a").get(0)
    val attr: Attributes = el.attributes
    assert(0== attr.size)
  }

  test("canStartWithEq") {
    val html: String = "<a =empty />"
    val el: Element = Jsoup.parse(html).getElementsByTag("a").get(0)
    val attr: Attributes = el.attributes
    assert(1== attr.size)
    assert(attr.hasKey("=empty"))
    assert(""== attr.get("=empty"))
  }

  test("strictAttributeUnescapes") {
    val html: String = "<a id=1 href='?foo=bar&mid&lt=true'>One</a> <a id=2 href='?foo=bar&lt;qux&lg=1'>Two</a>"
    val els: Elements = Jsoup.parse(html).select("a")
    assert("?foo=bar&mid&lt=true"== els.first().attr("href"))
    assert("?foo=bar<qux&lg=1"== els.last.attr("href"))
  }

  test("moreAttributeUnescapes") {
    val html: String = "<a href='&wr_id=123&mid-size=true&ok=&wr'>Check</a>"
    val els: Elements = Jsoup.parse(html).select("a")
    assert("&wr_id=123&mid-size=true&ok=&wr"== els.first.attr("href"))
  }
}
