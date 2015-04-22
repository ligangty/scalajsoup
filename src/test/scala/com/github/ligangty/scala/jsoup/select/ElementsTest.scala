package com.github.ligangty.scala.jsoup.select

import com.github.ligangty.scala.jsoup.parser.Tag
import com.github.ligangty.scala.jsoup.{TextUtil, Jsoup}
import com.github.ligangty.scala.jsoup.nodes._
import org.scalatest.FunSuite

import scala.collection.mutable

/**
 * Tests for ElementList.
 */
class ElementsTest extends FunSuite {


  test("filter") {
    val h: String = "<p>Excl</p><div class=headline><p>Hello</p><p>There</p></div><div class=headline><h1>Headline</h1></div>"
    val doc: Document = Jsoup.parse(h)
    val headlineEls = doc.select(".headline")
    val els: Elements = headlineEls.select("p")
    assert(2 == els.size)
    assert("Hello" == els.get(0).text)
    assert("There" == els.get(1).text)
  }

  test("attributes") {
    val h: String = "<p title=foo><p title=bar><p class=foo><p class=bar>"
    val doc: Document = Jsoup.parse(h)
    val withTitle: Elements = doc.select("p[title]")
    assert(2 == withTitle.size)
    assert(withTitle.hasAttr("title"))
    assert(!withTitle.hasAttr("class"))
    assert("foo" == withTitle.attr("title"))
    withTitle.removeAttr("title")
    assert(2 == withTitle.size)
    assert(0 == doc.select("p[title]").size)
    val ps: Elements = doc.select("p").attr("style", "classy")
    assert(4 == ps.size)
    assert("classy" == ps.last.attr("style"))
    assert("bar" == ps.last.attr("class"))
  }

  test("hasAttr") {
    val doc: Document = Jsoup.parse("<p title=foo><p title=bar><p class=foo><p class=bar>")
    val ps: Elements = doc.select("p")
    assert(ps.hasAttr("class"))
    assert(!ps.hasAttr("style"))
  }

  test("hasAbsAttr") {
    val doc: Document = Jsoup.parse("<a id=1 href='/foo'>One</a> <a id=2 href='http://jsoup.org'>Two</a>")
    val one: Elements = doc.select("#1")
    val two: Elements = doc.select("#2")
    val both: Elements = doc.select("a")
    assert(!one.hasAttr("abs:href"))
    assert(two.hasAttr("abs:href"))
    assert(both.hasAttr("abs:href"))
  }

  test("attr") {
    val doc: Document = Jsoup.parse("<p title=foo><p title=bar><p class=foo><p class=bar>")
    val classVal: String = doc.select("p").attr("class")
    assert("foo" == classVal)
  }

  test("absAttr") {
    val doc: Document = Jsoup.parse("<a id=1 href='/foo'>One</a> <a id=2 href='http://jsoup.org'>Two</a>")
    val one: Elements = doc.select("#1")
    val two: Elements = doc.select("#2")
    val both: Elements = doc.select("a")
    assert("" == one.attr("abs:href"))
    assert("http://jsoup.org" == two.attr("abs:href"))
    assert("http://jsoup.org" == both.attr("abs:href"))
  }

  test("classes") {
    val doc: Document = Jsoup.parse("<div><p class='mellow yellow'></p><p class='red green'></p>")
    val els: Elements = doc.select("p")
    assert(els.hasClass("red"))
    assert(!els.hasClass("blue"))
    els.addClass("blue")
    els.removeClass("yellow")
    els.toggleClass("mellow")
    assert("blue" == els.get(0).className)
    assert("red green blue mellow" == els.get(1).className)
  }

  test("text") {
    val h: String = "<div><p>Hello<p>there<p>world</div>"
    val doc: Document = Jsoup.parse(h)
    assert("Hello there world" == doc.select("div > *").text)
  }

  test("hasText") {
    val doc: Document = Jsoup.parse("<div><p>Hello</p></div><div><p></p></div>")
    val divs: Elements = doc.select("div")
    assert(divs.hasText)
    assert(!doc.select("div + div").hasText)
  }

  test("html") {
    val doc: Document = Jsoup.parse("<div><p>Hello</p></div><div><p>There</p></div>")
    val divs: Elements = doc.select("div")
    assert("<p>Hello</p>\n<p>There</p>" == divs.html)
  }

  test("outerHtml") {
    val doc: Document = Jsoup.parse("<div><p>Hello</p></div><div><p>There</p></div>")
    val divs: Elements = doc.select("div")
    assert("<div><p>Hello</p></div><div><p>There</p></div>" == TextUtil.stripNewlines(divs.outerHtml))
  }

  test("setHtml") {
    val doc: Document = Jsoup.parse("<p>One</p><p>Two</p><p>Three</p>")
    val ps: Elements = doc.select("p")
    ps.prepend("<b>Bold</b>").append("<i>Ital</i>")
    assert("<p><b>Bold</b>Two<i>Ital</i></p>" == TextUtil.stripNewlines(ps.get(1).outerHtml))
    ps.html("<span>Gone</span>")
    assert("<p><span>Gone</span></p>" == TextUtil.stripNewlines(ps.get(1).outerHtml))
  }

  test("value") {
    val doc: Document = Jsoup.parse("<input value='one' /><textarea>two</textarea>")
    val els: Elements = doc.select("input, textarea")
    assert(2 == els.size)
    assert("one" == els.value)
    assert("two" == els.last.value)
    els.value("three")
    assert("three" == els.first().value)
    assert("three" == els.last.value)
    assert("<textarea>three</textarea>" == els.last.outerHtml)
  }

  test("before") {
    val doc: Document = Jsoup.parse("<p>This <a>is</a> <a>jsoup</a>.</p>")
    doc.select("a").before("<span>foo</span>")
    assert("<p>This <span>foo</span><a>is</a> <span>foo</span><a>jsoup</a>.</p>" == TextUtil.stripNewlines(doc.body.html))
  }

  test("after") {
    val doc: Document = Jsoup.parse("<p>This <a>is</a> <a>jsoup</a>.</p>")
    doc.select("a").after("<span>foo</span>")
    assert("<p>This <a>is</a><span>foo</span> <a>jsoup</a><span>foo</span>.</p>" == TextUtil.stripNewlines(doc.body.html))
  }

  test("wrap") {
    val h: String = "<p><b>This</b> is <b>jsoup</b></p>"
    val doc: Document = Jsoup.parse(h)
    doc.select("b").wrap("<i></i>")
    assert("<p><i><b>This</b></i> is <i><b>jsoup</b></i></p>" == doc.body.html)
  }

  test("wrapDiv") {
    val h: String = "<p><b>This</b> is <b>jsoup</b>.</p> <p>How do you like it?</p>"
    val doc: Document = Jsoup.parse(h)
    doc.select("p").wrap("<div></div>")
    assert("<div><p><b>This</b> is <b>jsoup</b>.</p></div> <div><p>How do you like it?</p></div>" == TextUtil.stripNewlines(doc.body.html))
  }

  test("unwrap") {
    val h: String = "<div><font>One</font> <font><a href=\"/\">Two</a></font></div"
    val doc: Document = Jsoup.parse(h)
    doc.select("font").unwrap
    assert("<div>One <a href=\"/\">Two</a></div>" == TextUtil.stripNewlines(doc.body.html))
  }

  test("unwrapP") {
    val h: String = "<p><a>One</a> Two</p> Three <i>Four</i> <p>Fix <i>Six</i></p>"
    val doc: Document = Jsoup.parse(h)
    doc.select("p").unwrap
    assert("<a>One</a> Two Three <i>Four</i> Fix <i>Six</i>" == TextUtil.stripNewlines(doc.body.html))
  }

  test("empty") {
    val doc: Document = Jsoup.parse("<div><p>Hello <b>there</b></p> <p>now!</p></div>")
    doc.outputSettings.prettyPrint(false)
    doc.select("p").empty
    assert("<div><p></p> <p></p></div>" == doc.body.html)
  }

  test("remove") {
    val doc: Document = Jsoup.parse("<div><p>Hello <b>there</b></p> jsoup <p>now!</p></div>")
    doc.outputSettings.prettyPrint(false)
    doc.select("p").remove
    assert("<div> jsoup </div>" == doc.body.html)
  }

  test("eq") {
    val h: String = "<p>Hello<p>there<p>world"
    val doc: Document = Jsoup.parse(h)
    assert("there" == doc.select("p").eq(1).text)
    assert("there" == doc.select("p").get(1).text)
  }

  test("is") {
    val h: String = "<p>Hello<p title=foo>there<p>world"
    val doc: Document = Jsoup.parse(h)
    val ps: Elements = doc.select("p")
    assert(ps.is("[title=foo]"))
    assert(!ps.is("[title=bar]"))
  }

  test("parents") {
    val doc: Document = Jsoup.parse("<div><p>Hello</p></div><p>There</p>")
    val parents: Elements = doc.select("p").parents
    assert(3 == parents.size)
    assert("div" == parents.get(0).tagName)
    assert("body" == parents.get(1).tagName)
    assert("html" == parents.get(2).tagName)
  }

  test("not") {
    val doc: Document = Jsoup.parse("<div id=1><p>One</p></div> <div id=2><p><span>Two</span></p></div>")
    val div1: Elements = doc.select("div").not(":has(p > span)")
    assert(1 == div1.size)
    assert("1" == div1.first().id)
    val div2: Elements = doc.select("div").not("#1")
    assert(1 == div2.size)
    assert("2" == div2.first().id)
  }

  test("tagNameSet") {
    val doc: Document = Jsoup.parse("<p>Hello <i>there</i> <i>now</i></p>")
    doc.select("i").tagName("em")
    assert("<p>Hello <em>there</em> <em>now</em></p>" == doc.body.html)
  }

  test("traverse") {
    val doc: Document = Jsoup.parse("<div><p>Hello</p></div><div>There</div>")
    val accum: StringBuilder = new StringBuilder
    doc.select("div").traverse(new NodeVisitor {
      def head(node: Node, depth: Int) {
        accum.append("<" + node.nodeName + ">")
      }

      def tail(node: Node, depth: Int) {
        accum.append("</" + node.nodeName + ">")
      }
    })
    assert("<div><p><#text></#text></p></div><div><#text></#text></div>" == accum.toString)
  }

  test("forms") {
    val doc: Document = Jsoup.parse("<form id=1><input name=q></form><div /><form id=2><input name=f></form>")
    val els: Elements = doc.select("*")
    assert(9 == els.size)
    val forms: Seq[FormElement] = els.forms
    assert(2 == forms.size)
    assert(forms.head != null)
    assert(forms(1) != null)
    assert("1" == forms.head.id)
    assert("2" == forms(1).id)
  }

  test("classWithHyphen") {
    val doc: Document = Jsoup.parse("<p class='tab-nav'>Check</p>")
    val els: Elements = doc.getElementsByClass("tab-nav")
    assert(1 == els.size)
    assert("Check" == els.text)
  }
}
