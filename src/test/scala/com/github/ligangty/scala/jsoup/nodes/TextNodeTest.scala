package com.github.ligangty.scala.jsoup.nodes

import com.github.ligangty.scala.jsoup.{Jsoup, TextUtil}
import org.scalatest.FunSuite

/**
 * Test TextNodes
 */
class TextNodeTest extends FunSuite {

  test("testBlank") {
    val one: TextNode = new TextNode("", "")
    val two: TextNode = new TextNode("     ", "")
    val three: TextNode = new TextNode("  \n\n   ", "")
    val four: TextNode = new TextNode("Hello", "")
    val five: TextNode = new TextNode("  \nHello ", "")
    assert(one.isBlank)
    assert(two.isBlank)
    assert(three.isBlank)
    assert(!four.isBlank)
    assert(!five.isBlank)
  }

  test("testTextBean") {
    val doc: Document = Jsoup.parse("<p>One <span>two &amp;</span> three &amp;</p>")
    val p: Element = doc.select("p").first()
    val span: Element = doc.select("span").first()
    assert("two &" == span.text)
    val spanText: TextNode = span.getChildNode(0).asInstanceOf[TextNode]
    assert("two &" == spanText.text)
    val tn: TextNode = p.getChildNode(2).asInstanceOf[TextNode]
    assert(" three &" == tn.text)
    tn.text(" POW!")
    assert("One <span>two &amp;</span> POW!" == TextUtil.stripNewlines(p.html))
    tn.attr("text", "kablam &")
    assert("kablam &" == tn.text)
    assert("One <span>two &amp;</span>kablam &amp;" == TextUtil.stripNewlines(p.html))
  }

  test("testSplitText") {
    val doc: Document = Jsoup.parse("<div>Hello there</div>")
    val div: Element = doc.select("div").first()
    val tn: TextNode = div.getChildNode(0).asInstanceOf[TextNode]
    val tail: TextNode = tn.splitText(6)
    assert("Hello " == tn.getWholeText)
    assert("there" == tail.getWholeText)
    tail.text("there!")
    assert("Hello there!" == div.text)
    assert(tn.parent eq tail.parent)
  }

  test("testSplitAnEmbolden") {
    val doc: Document = Jsoup.parse("<div>Hello there</div>")
    val div: Element = doc.select("div").first()
    val tn: TextNode = div.getChildNode(0).asInstanceOf[TextNode]
    val tail: TextNode = tn.splitText(6)
    tail.wrap("<b></b>")
    assert("Hello <b>there</b>" == TextUtil.stripNewlines(div.html))
  }

  test("testWithSupplementaryCharacter") {
    val doc: Document = Jsoup.parse(new String(Character.toChars(135361)))
    val t: TextNode = doc.body.textNodes.head
    assert(new String(Character.toChars(135361)) == t.outerHtml.trim)
  }
}
