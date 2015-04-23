package com.github.ligangty.scala.jsoup.select

import com.github.ligangty.scala.jsoup.Jsoup
import com.github.ligangty.scala.jsoup.nodes.Document
import com.github.ligangty.scala.jsoup.parser.Tag
import org.scalatest._

class CssTest extends FunSuite with BeforeAndAfterAll with BeforeAndAfterEach with MustMatchers {

  private var html: Document = null
  private var htmlString: String = null

  override def beforeAll() {
    val sb: StringBuilder = new StringBuilder("<html><head></head><body>")

    sb.append("<div id='pseudo'>")
    for (i <- 1 to 10) {
      sb.append("<p>%d</p>".format(i))
    }
    sb.append("</div>")

    sb.append("<div id='type'>")
    for (i <- 1 to 10) {
      sb.append("<p>%d</p>".format(i))
              .append("<span>%d</span>".format(i))
              .append("<em>%d</em>".format(i))
              .append("<svg>%d</svg>".format(i))
    }
    sb.append("</div>")

    sb.append("<span id='onlySpan'><br /></span>")
    sb.append("<p class='empty'><!-- Comment only is still empty! --></p>")

    sb.append("<div id='only'>")
    sb.append("Some text before the <em>only</em> child in this div")
    sb.append("</div>")

    sb.append("</body></html>")
    htmlString = sb.toString()
  }

  override def beforeEach(): Unit = {
    html = Jsoup.parse(htmlString)
  }

  test("firstChild") {
    check(html.select("#pseudo :first-child"), "1")
    check(html.select("html:first-child"))
  }

  test("lastChild") {
    check(html.select("#pseudo :last-child"), "10")
    check(html.select("html:last-child"))
  }

  test("nthChild_simple") {
    for (i <- 1 to 10) {
      check(html.select("#pseudo :nth-child(%d)".format(i)), String.valueOf(i))
    }
  }

  test("nthOfType_unknownTag") {
    for (i <- 1 to 10) {
      check(html.select("#type svg:nth-of-type(%d)".format(i)), String.valueOf(i))
    }
  }

  test("nthLastChild_simple") {
    for (i <- 1 to 10) {
      check(html.select("#pseudo :nth-last-child(%d)".format(i)), String.valueOf(11 - i))
    }
  }

  test("nthOfType_simple") {
    for (i <- 1 to 10) {
      check(html.select("#type p:nth-of-type(%d)".format(i)), String.valueOf(i))
    }
  }

  test("nthLastOfType_simple") {
    for (i <- 1 to 10) {
      check(html.select("#type :nth-last-of-type(%d)".format(i)), String.valueOf(11 - i), String.valueOf(11 - i), String.valueOf(11 - i), String.valueOf(11 - i))
    }
  }

  test("nthChild_advanced") {
    check(html.select("#pseudo :nth-child(-5)"))
    check(html.select("#pseudo :nth-child(odd)"), "1", "3", "5", "7", "9")
    check(html.select("#pseudo :nth-child(2n-1)"), "1", "3", "5", "7", "9")
    check(html.select("#pseudo :nth-child(2n+1)"), "1", "3", "5", "7", "9")
    check(html.select("#pseudo :nth-child(2n+3)"), "3", "5", "7", "9")
    check(html.select("#pseudo :nth-child(even)"), "2", "4", "6", "8", "10")
    check(html.select("#pseudo :nth-child(2n)"), "2", "4", "6", "8", "10")
    check(html.select("#pseudo :nth-child(3n-1)"), "2", "5", "8")
    check(html.select("#pseudo :nth-child(-2n+5)"), "1", "3", "5")
    check(html.select("#pseudo :nth-child(+5)"), "5")
  }

  test("nthOfType_advanced") {
    check(html.select("#type :nth-of-type(-5)"))
    check(html.select("#type p:nth-of-type(odd)"), "1", "3", "5", "7", "9")
    check(html.select("#type em:nth-of-type(2n-1)"), "1", "3", "5", "7", "9")
    check(html.select("#type p:nth-of-type(2n+1)"), "1", "3", "5", "7", "9")
    check(html.select("#type span:nth-of-type(2n+3)"), "3", "5", "7", "9")
    check(html.select("#type p:nth-of-type(even)"), "2", "4", "6", "8", "10")
    check(html.select("#type p:nth-of-type(2n)"), "2", "4", "6", "8", "10")
    check(html.select("#type p:nth-of-type(3n-1)"), "2", "5", "8")
    check(html.select("#type p:nth-of-type(-2n+5)"), "1", "3", "5")
    check(html.select("#type :nth-of-type(+5)"), "5", "5", "5", "5")
  }

  test("nthLastChild_advanced") {
    check(html.select("#pseudo :nth-last-child(-5)"))
    check(html.select("#pseudo :nth-last-child(odd)"), "2", "4", "6", "8", "10")
    check(html.select("#pseudo :nth-last-child(2n-1)"), "2", "4", "6", "8", "10")
    check(html.select("#pseudo :nth-last-child(2n+1)"), "2", "4", "6", "8", "10")
    check(html.select("#pseudo :nth-last-child(2n+3)"), "2", "4", "6", "8")
    check(html.select("#pseudo :nth-last-child(even)"), "1", "3", "5", "7", "9")
    check(html.select("#pseudo :nth-last-child(2n)"), "1", "3", "5", "7", "9")
    check(html.select("#pseudo :nth-last-child(3n-1)"), "3", "6", "9")
    check(html.select("#pseudo :nth-last-child(-2n+5)"), "6", "8", "10")
    check(html.select("#pseudo :nth-last-child(+5)"), "6")
  }

  test("nthLastOfType_advanced") {
    check(html.select("#type :nth-last-of-type(-5)"))
    check(html.select("#type p:nth-last-of-type(odd)"), "2", "4", "6", "8", "10")
    check(html.select("#type em:nth-last-of-type(2n-1)"), "2", "4", "6", "8", "10")
    check(html.select("#type p:nth-last-of-type(2n+1)"), "2", "4", "6", "8", "10")
    check(html.select("#type span:nth-last-of-type(2n+3)"), "2", "4", "6", "8")
    check(html.select("#type p:nth-last-of-type(even)"), "1", "3", "5", "7", "9")
    check(html.select("#type p:nth-last-of-type(2n)"), "1", "3", "5", "7", "9")
    check(html.select("#type p:nth-last-of-type(3n-1)"), "3", "6", "9")
    check(html.select("#type span:nth-last-of-type(-2n+5)"), "6", "8", "10")
    check(html.select("#type :nth-last-of-type(+5)"), "6", "6", "6", "6")
  }

  test("firstOfType") {
    check(html.select("div:not(#only) :first-of-type"), "1", "1", "1", "1", "1")
  }

  test("lastOfType") {
    check(html.select("div:not(#only) :last-of-type"), "10", "10", "10", "10", "10")
  }

  test("empty") {
    val sel: Elements = html.select(":empty")
    assert(3 == sel.size)
    assert("head" == sel.get(0).tagName)
    assert("br" == sel.get(1).tagName)
    assert("p" == sel.get(2).tagName)
  }

  test("onlyChild") {
    val sel: Elements = html.select("span :only-child")
    assert(1 == sel.size)
    assert("br" == sel.get(0).tagName)
    check(html.select("#only :only-child"), "only")
  }

  test("onlyOfType") {
    val sel: Elements = html.select(":only-of-type")
    assert(6 == sel.size)
    assert("head" == sel.get(0).tagName)
    assert("body" == sel.get(1).tagName)
    assert("span" == sel.get(2).tagName)
    assert("br" == sel.get(3).tagName)
    assert("p" == sel.get(4).tagName)
    assert(sel.get(4).hasClass("empty"))
    assert("em" == sel.get(5).tagName)
  }

  protected def check(result: Elements, expectedContent: String*) {
    assert(expectedContent.length == result.size, "Number of elements")
    for (i <- 0 to expectedContent.length - 1) {
      assert(result.get(i) != null)
      assert(expectedContent(i) == result.get(i).ownText, "Expected element")
    }
  }

  test("root") {
    val sel: Elements = html.select(":root")
    assert(1 == sel.size)
    assert(sel.get(0) != null)
    assert(Tag("html") == sel.get(0).tag)
    val sel2: Elements = html.select("body").select(":root")
    assert(1 == sel2.size)
    assert(sel2.get(0) != null)
    assert(Tag("body") == sel2.get(0).tag)
  }
}
