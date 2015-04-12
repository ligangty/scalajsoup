package com.github.ligangty.scala.jsoup.nodes

import com.github.ligangty.scala.jsoup.select.NodeVisitor
import com.github.ligangty.scala.jsoup.{TextUtil, Jsoup}
import com.github.ligangty.scala.jsoup.parser.Tag
import org.scalatest.FunSuite

/**
  */
class NodeTest extends FunSuite {

  test("handlesBaseUri") {
    val tag: Tag = Tag("a")
    val attribs: Attributes = new Attributes
    attribs.put("relHref", "/foo")
    attribs.put("absHref", "http://bar/qux")
    val noBase: Element = new Element(tag, "", attribs)
    assert("" == noBase.absUrl("relHref"))
    assert("http://bar/qux" == noBase.absUrl("absHref"))
    val withBase: Element = new Element(tag, "http://foo/", attribs)
    assert("http://foo/foo" == withBase.absUrl("relHref"))
    assert("http://bar/qux" == withBase.absUrl("absHref"))
    assert("" == withBase.absUrl("noval"))
    val dodgyBase: Element = new Element(tag, "wtf://no-such-protocol/", attribs)
    assert("http://bar/qux" == dodgyBase.absUrl("absHref"))
    assert("" == dodgyBase.absUrl("relHref"))
  }

  test("setBaseUriIsRecursive") {
    val doc: Document = Jsoup.parse("<div><p></p></div>")
    val baseUri: String = "http://jsoup.org"
    doc.setBaseUri(baseUri)
    assert(baseUri == doc.baseUri)
    assert(baseUri == doc.select("div").first().baseUri)
    assert(baseUri == doc.select("p").first().baseUri)
  }

  test("handlesAbsPrefix") {
    val doc: Document = Jsoup.parse("<a href=/foo>Hello</a>", "http://jsoup.org/")
    val a: Element = doc.select("a").first()
    assert("/foo" == a.attr("href"))
    assert("http://jsoup.org/foo" == a.attr("abs:href"))
    assert(a.hasAttr("abs:href"))
  }

  test("handlesAbsOnImage") {
    val doc: Document = Jsoup.parse("<p><img src=\"/rez/osi_logo.png\" /></p>", "http://jsoup.org/")
    val img: Element = doc.select("img").first()
    assert("http://jsoup.org/rez/osi_logo.png" == img.attr("abs:src"))
    assert(img.absUrl("src") == img.attr("abs:src"))
  }

  test("handlesAbsPrefixOnHasAttr") {
    val doc: Document = Jsoup.parse("<a id=1 href='/foo'>One</a> <a id=2 href='http://jsoup.org/'>Two</a>")
    val one: Element = doc.select("#1").first()
    val two: Element = doc.select("#2").first()
    assert(!one.hasAttr("abs:href"))
    assert(one.hasAttr("href"))
    assert("" == one.absUrl("href"))
    assert(two.hasAttr("abs:href"))
    assert(two.hasAttr("href"))
    assert("http://jsoup.org/" == two.absUrl("href"))
  }

  test("literalAbsPrefix") {
    val doc: Document = Jsoup.parse("<a abs:href='odd'>One</a>")
    val el: Element = doc.select("a").first()
    assert(el.hasAttr("abs:href"))
    assert("odd" == el.attr("abs:href"))
  }

  test("handleAbsOnFileUris") {
    val doc: Document = Jsoup.parse("<a href='password'>One/a><a href='/var/log/messages'>Two</a>", "file:/etc/")
    val one: Element = doc.select("a").first()
    assert("file:/etc/password" == one.absUrl("href"))
    val two: Element = doc.select("a").get(1)
    assert("file:/var/log/messages" == two.absUrl("href"))
  }

  test("handleAbsOnLocalhostFileUris") {
    val doc: Document = Jsoup.parse("<a href='password'>One/a><a href='/var/log/messages'>Two</a>", "file://localhost/etc/")
    val one: Element = doc.select("a").first()
    assert("file://localhost/etc/password" == one.absUrl("href"))
  }

  test("handlesAbsOnProtocolessAbsoluteUris") {
    val doc1: Document = Jsoup.parse("<a href='//example.net/foo'>One</a>", "http://example.com/")
    val doc2: Document = Jsoup.parse("<a href='//example.net/foo'>One</a>", "https://example.com/")
    val one: Element = doc1.select("a").first()
    val two: Element = doc2.select("a").first()
    assert("http://example.net/foo" == one.absUrl("href"))
    assert("https://example.net/foo" == two.absUrl("href"))
    val doc3: Document = Jsoup.parse("<img src=//www.google.com/images/errors/logo_sm.gif alt=Google>", "https://google.com")
    assert("https://www.google.com/images/errors/logo_sm.gif" == doc3.select("img").attr("abs:src"))
  }

  test("absHandlesRelativeQuery") {
    val doc: Document = Jsoup.parse("<a href='?foo'>One</a> <a href='bar.html?foo'>Two</a>", "http://jsoup.org/path/file?bar")
    val a1: Element = doc.select("a").first()
    assert("http://jsoup.org/path/file?foo" == a1.absUrl("href"))
    val a2: Element = doc.select("a").get(1)
    assert("http://jsoup.org/path/bar.html?foo" == a2.absUrl("href"))
  }

  test("testRemove") {
    val doc: Document = Jsoup.parse("<p>One <span>two</span> three</p>")
    val p: Element = doc.select("p").first()
    p.getChildNode(0).remove()
    assert("two three" == p.text)
    assert("<span>two</span> three" == TextUtil.stripNewlines(p.html))
  }

  test("testReplace") {
    val doc: Document = Jsoup.parse("<p>One <span>two</span> three</p>")
    val p: Element = doc.select("p").first()
    val insert: Element = doc.createElement("em").text("foo")
    p.getChildNode(1).replaceWith(insert)
    assert("One <em>foo</em> three" == p.html)
  }

  test("ownerDocument") {
    val doc: Document = Jsoup.parse("<p>Hello")
    val p: Element = doc.select("p").first()
    assert(p.ownerDocument eq doc)
    assert(doc.ownerDocument eq doc)
    assert(doc.parent == null)
  }

  test("before") {
    val doc: Document = Jsoup.parse("<p>One <b>two</b> three</p>")
    val newNode: Element = new Element(Tag("em"), "")
    newNode.appendText("four")
    doc.select("b").first().before(newNode)
    assert("<p>One <em>four</em><b>two</b> three</p>" == doc.body.html)
    doc.select("b").first().before("<i>five</i>")
    assert("<p>One <em>four</em><i>five</i><b>two</b> three</p>" == doc.body.html)
  }

  test("after") {
    val doc: Document = Jsoup.parse("<p>One <b>two</b> three</p>")
    val newNode: Element = new Element(Tag("em"), "")
    newNode.appendText("four")
    doc.select("b").first().after(newNode)
    assert("<p>One <b>two</b><em>four</em> three</p>" == doc.body.html)
    doc.select("b").first().after("<i>five</i>")
    assert("<p>One <b>two</b><i>five</i><em>four</em> three</p>" == doc.body.html)
  }

  test("unwrap") {
    val doc: Document = Jsoup.parse("<div>One <span>Two <b>Three</b></span> Four</div>")
    val span: Element = doc.select("span").first()
    val twoText: Node = span.getChildNode(0)
    val node: Node = span.unwrap
    assert("<div>One Two <b>Three</b> Four</div>" == TextUtil.stripNewlines(doc.body.html))
    assert(node.isInstanceOf[TextNode])
    assert("Two " == node.asInstanceOf[TextNode].text())
    assert(node == twoText)
    assert(node.parent == doc.select("div").first())
  }

  test("unwrapNoChildren") {
    val doc: Document = Jsoup.parse("<div>One <span></span> Two</div>")
    val span: Element = doc.select("span").first()
    val node: Node = span.unwrap
    assert("<div>One  Two</div>" == TextUtil.stripNewlines(doc.body.html))
    assert(node == null)
  }

  test("traverse") {
    val doc: Document = Jsoup.parse("<div><p>Hello</p></div><div>There</div>")
    val accum: StringBuilder = new StringBuilder
    doc.select("div").first().traverse(new NodeVisitor {
      def head(node: Node, depth: Int) {
        accum.append("<" + node.nodeName + ">")
      }

      def tail(node: Node, depth: Int) {
        accum.append("</" + node.nodeName + ">")
      }
    })
    assert("<div><p><#text></#text></p></div>" == accum.toString)
  }

  test("orphanNodeReturnsNullForSiblingElements") {
    val node: Node = new Element(Tag("p"), "")
    val el: Element = new Element(Tag("p"), "")
    assert(0 == node.siblingIndex)
    assert(0 == node.siblingNodes.size)
    assert(node.previousSibling == null)
    assert(node.nextSibling == null)
    assert(0 == el.siblingElements.size)
    assert(el.previousElementSibling == null)
    assert(el.nextElementSibling == null)
  }

  test("nodeIsNotASiblingOfItself") {
    val doc: Document = Jsoup.parse("<div><p>One<p>Two<p>Three</div>")
    val p2: Element = doc.select("p").get(1)
    assert("Two" == p2.text)
    val nodes: List[Node] = p2.siblingNodes
    assert(2 == nodes.size)
    assert("<p>One</p>" == nodes(0).outerHtml)
    assert("<p>Three</p>" == nodes(1).outerHtml)
  }

  test("childNodesCopy") {
    val doc: Document = Jsoup.parse("<div id=1>Text 1 <p>One</p> Text 2 <p>Two<p>Three</div><div id=2>")
    val div1: Element = doc.select("#1").first()
    val div2: Element = doc.select("#2").first()
    val divChildren: Seq[Node] = div1.childNodesCopy
    assert(5 == divChildren.size)
    val tn1: TextNode = div1.getChildNode(0).asInstanceOf[TextNode]
    val tn2: TextNode = divChildren(0).asInstanceOf[TextNode]
    tn2.text("Text 1 updated")
    assert("Text 1 " == tn1.text)
    div2.insertChildren(-1, divChildren)
    assert("<div id=\"1\">Text 1 <p>One</p> Text 2 <p>Two</p><p>Three</p></div><div id=\"2\">Text 1 updated" + "<p>One</p> Text 2 <p>Two</p><p>Three</p></div>" == TextUtil.stripNewlines(doc.body.html))
  }
}
