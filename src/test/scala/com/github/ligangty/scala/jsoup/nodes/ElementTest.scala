package com.github.ligangty.scala.jsoup.nodes

import com.github.ligangty.scala.jsoup.{TextUtil, Jsoup}
import com.github.ligangty.scala.jsoup.parser.Tag
import com.github.ligangty.scala.jsoup.select.Elements
import org.scalatest.FunSuite
import scala.collection.mutable

/**
 * Tests for Element (DOM stuff mostly).
 */
class ElementTest extends FunSuite {

  private val reference: String = "<div id=div1><p>Hello</p><p>Another <b>element</b></p><div id=div2><img src=foo.png></div></div>"

  test("getElementsByTagName") {
    val doc: Document = Jsoup.parse(reference)
    val divs: mutable.Seq[Element] = doc.getElementsByTag("div")
    assert(2 == divs.size)
    assert("div1" == divs(0).id)
    assert("div2" == divs(1).id)
    val ps: mutable.Seq[Element] = doc.getElementsByTag("p")
    assert(2 == ps.size)
    assert("Hello" == ps(0).getChildNode(0).asInstanceOf[TextNode].getWholeText)
    assert("Another " == ps(1).getChildNode(0).asInstanceOf[TextNode].getWholeText)
    val ps2: mutable.Seq[Element] = doc.getElementsByTag("P")
    assert(ps == ps2)
    val imgs: mutable.Seq[Element] = doc.getElementsByTag("img")
    assert("foo.png" == imgs(0).attr("src"))
    val empty: mutable.Seq[Element] = doc.getElementsByTag("wtf")
    assert(0 == empty.size)
  }

  test("getNamespacedElementsByTag") {
    val doc: Document = Jsoup.parse("<div><abc:def id=1>Hello</abc:def></div>")
    val els: Elements = doc.getElementsByTag("abc:def")
    assert(1 == els.size)
    assert("1" == els.first().id)
    assert("abc:def" == els.first().tagName)
  }

  test("testGetElementById") {
    val doc: Document = Jsoup.parse(reference)
    val div: Element = doc.getElementById("div1")
    assert("div1" == div.id)
    assert(doc.getElementById("none") == null)
    val doc2: Document = Jsoup.parse("<div id=1><div id=2><p>Hello <span id=2>world!</span></p></div></div>")
    val div2: Element = doc2.getElementById("2")
    assert("div" == div2.tagName)
    val span: Element = div2.child(0).getElementById("2")
    assert("span" == span.tagName)
  }

  test("testGetText") {
    val doc: Document = Jsoup.parse(reference)
    assert("Hello Another element" == doc.text())
    assert("Another element" == doc.getElementsByTag("p")(1).text())
  }

  test("testGetChildText") {
    val doc: Document = Jsoup.parse("<p>Hello <b>there</b> now")
    val p: Element = doc.select("p").first()
    assert("Hello there now" == p.text())
    assert("Hello now" == p.ownText)
  }

  test("testNormalisesText") {
    val h: String = "<p>Hello<p>There.</p> \n <p>Here <b>is</b> \n s<b>om</b>e text."
    val doc: Document = Jsoup.parse(h)
    val text: String = doc.text()
    assert("Hello There. Here is some text." == text)
  }

  test("testKeepsPreText") {
    val h: String = "<p>Hello \n \n there.</p> <div><pre>  What's \n\n  that?</pre>"
    val doc: Document = Jsoup.parse(h)
    assert("Hello there.   What's \n\n  that?" == doc.text())
  }

  test("testKeepsPreTextInCode") {
    val h: String = "<pre><code>code\n\ncode</code></pre>"
    val doc: Document = Jsoup.parse(h)
    assert("code\n\ncode" == doc.text())
    assert("<pre><code>code\n\ncode</code></pre>" == doc.body.html)
  }

  test("testBrHasSpace") {
    var doc: Document = Jsoup.parse("<p>Hello<br>there</p>")
    assert("Hello there" == doc.text())
    assert("Hello there" == doc.select("p").first().ownText)
    doc = Jsoup.parse("<p>Hello <br> there</p>")
    assert("Hello there" == doc.text())
  }

  test("testGetSiblings") {
    val doc: Document = Jsoup.parse("<div><p>Hello<p id=1>there<p>this<p>is<p>an<p id=last>element</div>")
    val p: Element = doc.getElementById("1")
    assert("there" == p.text())
    assert("Hello" == p.previousElementSibling.text())
    assert("this" == p.nextElementSibling.text())
    assert("Hello" == p.firstElementSibling.text())
    assert("element" == p.lastElementSibling.text())
  }

  test("testGetParents") {
    val doc: Document = Jsoup.parse("<div><p>Hello <span>there</span></div>")
    val span: Element = doc.select("span").first()
    val parents: Elements = span.parents
    assert(4 == parents.size)
    assert("p" == parents.get(0).tagName)
    assert("div" == parents.get(1).tagName)
    assert("body" == parents.get(2).tagName)
    assert("html" == parents.get(3).tagName)
  }

  test("testElementSiblingIndex") {
    val doc: Document = Jsoup.parse("<div><p>One</p>...<p>Two</p>...<p>Three</p>")
    val ps: Elements = doc.select("p")
    assert(0 == ps.get(0).elementSiblingIndex)
    assert(1 == ps.get(1).elementSiblingIndex)
    assert(2 == ps.get(2).elementSiblingIndex)
  }

  test("testGetElementsWithClass") {
    val doc: Document = Jsoup.parse("<div class='mellow yellow'><span class=mellow>Hello <b class='yellow'>Yellow!</b></span><p>Empty</p></div>")
    val els: mutable.Seq[Element] = doc.getElementsByClass("mellow")
    assert(2 == els.size)
    assert("div" == els(0).tagName)
    assert("span" == els(1).tagName)
    val els2: mutable.Seq[Element] = doc.getElementsByClass("yellow")
    assert(2 == els2.size)
    assert("div" == els2(0).tagName)
    assert("b" == els2(1).tagName)
    val none: mutable.Seq[Element] = doc.getElementsByClass("solo")
    assert(0 == none.size)
  }

  test("testGetElementsWithAttribute") {
    val doc: Document = Jsoup.parse("<div style='bold'><p title=qux><p><b style></b></p></div>")
    val els: mutable.Seq[Element] = doc.getElementsByAttribute("style")
    assert(2 == els.size)
    assert("div" == els(0).tagName)
    assert("b" == els(1).tagName)
    val none: mutable.Seq[Element] = doc.getElementsByAttribute("class")
    assert(0 == none.size)
  }

  test("testGetElementsWithAttributeDash") {
    val doc: Document = Jsoup.parse("<meta http-equiv=content-type value=utf8 id=1> <meta name=foo content=bar id=2> <div http-equiv=content-type value=utf8 id=3>")
    val meta: Elements = doc.select("meta[http-equiv=content-type], meta[charset]")
    assert(1 == meta.size)
    assert("1" == meta.first().id)
  }

  test("testGetElementsWithAttributeValue") {
    val doc: Document = Jsoup.parse("<div style='bold'><p><p><b style></b></p></div>")
    val els: mutable.Seq[Element] = doc.getElementsByAttributeValue("style", "bold")
    assert(1 == els.size)
    assert("div" == els(0).tagName)
    val none: mutable.Seq[Element] = doc.getElementsByAttributeValue("style", "none")
    assert(0 == none.size)
  }

  test("testClassDomMethods") {
    val doc: Document = Jsoup.parse("<div><span class=' mellow yellow '>Hello <b>Yellow</b></span></div>")
    val els: mutable.Seq[Element] = doc.getElementsByAttribute("class")
    val span: Element = els(0)
    assert("mellow yellow" == span.className)
    assert(span.hasClass("mellow"))
    assert(span.hasClass("yellow"))
    var classes: Set[String] = span.classNames
    assert(2 == classes.size)
    assert(classes.contains("mellow"))
    assert(classes.contains("yellow"))
    assert("" == doc.className)
    classes = doc.classNames
    assert(0 == classes.size)
    assert(!doc.hasClass("mellow"))
  }

  test("testClassUpdates") {
    val doc: Document = Jsoup.parse("<div class='mellow yellow'></div>")
    val div: Element = doc.select("div").first()
    div.addClass("green")
    assert("mellow yellow green" == div.className)
    div.removeClass("red")
    div.removeClass("yellow")
    assert("mellow green" == div.className)
    div.toggleClass("green").toggleClass("red")
    assert("mellow red" == div.className)
  }

  test("testOuterHtml") {
    val doc: Document = Jsoup.parse("<div title='Tags &amp;c.'><img src=foo.png><p><!-- comment -->Hello<p>there")
    assert("<html><head></head><body><div title=\"Tags &amp;c.\"><img src=\"foo.png\"><p><!-- comment -->Hello</p><p>there</p></div></body></html>" == TextUtil.stripNewlines(doc.outerHtml))
  }

  test("testInnerHtml") {
    val doc: Document = Jsoup.parse("<div>\n <p>Hello</p> </div>")
    assert("<p>Hello</p>" == doc.getElementsByTag("div").get(0).html)
  }

  test("testFormatHtml") {
    val doc: Document = Jsoup.parse("<title>Format test</title><div><p>Hello <span>jsoup <span>users</span></span></p><p>Good.</p></div>")
    assert("<html>\n <head>\n  <title>Format test</title>\n </head>\n <body>\n  <div>\n   <p>Hello <span>jsoup <span>users</span></span></p>\n   <p>Good.</p>\n  </div>\n </body>\n</html>" == doc.html)
  }

  test("testFormatOutline") {
    val doc: Document = Jsoup.parse("<title>Format test</title><div><p>Hello <span>jsoup <span>users</span></span></p><p>Good.</p></div>")
    doc.outputSettings.outline(true)
    assert("<html>\n <head>\n  <title>Format test</title>\n </head>\n <body>\n  <div>\n   <p>\n    Hello \n    <span>\n     jsoup \n     <span>users</span>\n    </span>\n   </p>\n   <p>Good.</p>\n  </div>\n </body>\n</html>" == doc.html)
  }

  test("testSetIndent") {
    val doc: Document = Jsoup.parse("<div><p>Hello\nthere</p></div>")
    doc.outputSettings.indentAmount(0)
    assert("<html>\n<head></head>\n<body>\n<div>\n<p>Hello there</p>\n</div>\n</body>\n</html>" == doc.html)
  }

  test("testNotPretty") {
    val doc: Document = Jsoup.parse("<div>   \n<p>Hello\n there\n</p></div>")
    doc.outputSettings.prettyPrint(false)
    assert("<html><head></head><body><div>   \n<p>Hello\n there\n</p></div></body></html>" == doc.html)
    val div: Element = doc.select("div").first()
    assert("   \n<p>Hello\n there\n</p>" == div.html)
  }

  test("testEmptyElementFormatHtml") {
    val doc: Document = Jsoup.parse("<section><div></div></section>")
    assert("<section>\n <div></div>\n</section>" == doc.select("section").first().outerHtml)
  }

  test("testNoIndentOnScriptAndStyle") {
    val doc: Document = Jsoup.parse("<script>one\ntwo</script>\n<style>three\nfour</style>")
    assert("<script>one\ntwo</script> \n<style>three\nfour</style>" == doc.head.html)
  }

  test("testContainerOutput") {
    val doc: Document = Jsoup.parse("<title>Hello there</title> <div><p>Hello</p><p>there</p></div> <div>Another</div>")
    assert("<title>Hello there</title>" == doc.select("title").first().outerHtml)
    assert("<div>\n <p>Hello</p>\n <p>there</p>\n</div>" == doc.select("div").first().outerHtml)
    assert("<div>\n <p>Hello</p>\n <p>there</p>\n</div> \n<div>\n Another\n</div>" == doc.select("body").first().html)
  }

  test("testSetText") {
    val h: String = "<div id=1>Hello <p>there <b>now</b></p></div>"
    val doc: Document = Jsoup.parse(h)
    assert("Hello there now" == doc.text())
    assert("there now" == doc.select("p").get(0).text())
    val div: Element = doc.getElementById("1").text("Gone")
    assert("Gone" == div.text())
    assert(0 == doc.select("p").size)
  }

  test("testAddNewElement") {
    val doc: Document = Jsoup.parse("<div id=1><p>Hello</p></div>")
    val div: Element = doc.getElementById("1")
    div.appendElement("p").text("there")
    div.appendElement("P").attr("class", "second").text("now")
    assert("<html><head></head><body><div id=\"1\"><p>Hello</p><p>there</p><p class=\"second\">now</p></div></body></html>" == TextUtil.stripNewlines(doc.html))
    // check sibling index (with short circuit on reindexChildren):
    val ps: Elements = doc.select("p")
    for (i <- 0 to (ps.size - 1)) {
      assert(i == ps.get(i).siblingIndex)
    }
  }

  test("testAppendRowToTable") {
    val doc: Document = Jsoup.parse("<table><tr><td>1</td></tr></table>")
    val table: Element = doc.select("tbody").first()
    table.append("<tr><td>2</td></tr>")
    assert("<table><tbody><tr><td>1</td></tr><tr><td>2</td></tr></tbody></table>" == TextUtil.stripNewlines(doc.body.html))
  }

  test("testPrependRowToTable") {
    val doc: Document = Jsoup.parse("<table><tr><td>1</td></tr></table>")
    val table: Element = doc.select("tbody").first()
    table.prepend("<tr><td>2</td></tr>")
    assert("<table><tbody><tr><td>2</td></tr><tr><td>1</td></tr></tbody></table>" == TextUtil.stripNewlines(doc.body.html))
    // check sibling index (reindexChildren):
    val ps: Elements = doc.select("tr")
    for (i <- 0 to (ps.size - 1)) {
      assert(i == ps.get(i).siblingIndex)
    }
  }

  test("testPrependElement") {
    val doc: Document = Jsoup.parse("<div id=1><p>Hello</p></div>")
    val div: Element = doc.getElementById("1")
    div.prependElement("p").text("Before")
    assert("Before" == div.child(0).text())
    assert("Hello" == div.child(1).text())
  }

  test("testAddNewText") {
    val doc: Document = Jsoup.parse("<div id=1><p>Hello</p></div>")
    val div: Element = doc.getElementById("1")
    div.appendText(" there & now >")
    assert("<p>Hello</p> there &amp; now &gt;" == TextUtil.stripNewlines(div.html))
  }

  test("testPrependText") {
    val doc: Document = Jsoup.parse("<div id=1><p>Hello</p></div>")
    val div: Element = doc.getElementById("1")
    div.prependText("there & now > ")
    assert("there & now > Hello" == div.text())
    assert("there &amp; now &gt; <p>Hello</p>" == TextUtil.stripNewlines(div.html))
  }

  test("testAddNewHtml") {
    val doc: Document = Jsoup.parse("<div id=1><p>Hello</p></div>")
    val div: Element = doc.getElementById("1")
    div.append("<p>there</p><p>now</p>")
    assert("<p>Hello</p><p>there</p><p>now</p>" == TextUtil.stripNewlines(div.html))
    // check sibling index (no reindexChildren):
    val ps: Elements = doc.select("p")
    for (i <- 0 to (ps.size - 1)) {
      assert(i == ps.get(i).siblingIndex)
    }
  }

  test("testPrependNewHtml") {
    val doc: Document = Jsoup.parse("<div id=1><p>Hello</p></div>")
    val div: Element = doc.getElementById("1")
    div.prepend("<p>there</p><p>now</p>")
    assert("<p>there</p><p>now</p><p>Hello</p>" == TextUtil.stripNewlines(div.html))
    // check sibling index (reindexChildren):
    val ps: Elements = doc.select("p")
    for (i <- 0 to (ps.size - 1)) {
      assert(i == ps.get(i).siblingIndex)
    }
  }

  test("testSetHtml") {
    val doc: Document = Jsoup.parse("<div id=1><p>Hello</p></div>")
    val div: Element = doc.getElementById("1")
    div.html("<p>there</p><p>now</p>")
    assert("<p>there</p><p>now</p>" == TextUtil.stripNewlines(div.html))
  }

  test("testSetHtmlTitle") {
    val doc: Document = Jsoup.parse("<html><head id=2><title id=1></title></head></html>")
    val title: Element = doc.getElementById("1")
    title.html("good")
    assert("good" == title.html)
    title.html("<i>bad</i>")
    assert("&lt;i&gt;bad&lt;/i&gt;" == title.html)
    val head: Element = doc.getElementById("2")
    head.html("<title><i>bad</i></title>")
    assert("<title>&lt;i&gt;bad&lt;/i&gt;</title>" == head.html)
  }

  test("testWrap") {
    val doc: Document = Jsoup.parse("<div><p>Hello</p><p>There</p></div>")
    val p: Element = doc.select("p").first()
    p.wrap("<div class='head'></div>")
    assert("<div><div class=\"head\"><p>Hello</p></div><p>There</p></div>" == TextUtil.stripNewlines(doc.body.html))
    val ret: Element = p.wrap("<div><div class=foo></div><p>What?</p></div>")
    assert("<div><div class=\"head\"><div><div class=\"foo\"><p>Hello</p></div><p>What?</p></div></div><p>There</p></div>" == TextUtil.stripNewlines(doc.body.html))
    assert(ret == p)
  }

  test("before") {
    val doc: Document = Jsoup.parse("<div><p>Hello</p><p>There</p></div>")
    val p1: Element = doc.select("p").first()
    p1.before("<div>one</div><div>two</div>")
    assert("<div><div>one</div><div>two</div><p>Hello</p><p>There</p></div>" == TextUtil.stripNewlines(doc.body.html))
    doc.select("p").last.before("<p>Three</p><!-- four -->")
    assert("<div><div>one</div><div>two</div><p>Hello</p><p>Three</p><!-- four --><p>There</p></div>" == TextUtil.stripNewlines(doc.body.html))
  }

  test("after") {
    val doc: Document = Jsoup.parse("<div><p>Hello</p><p>There</p></div>")
    val p1: Element = doc.select("p").first()
    p1.after("<div>one</div><div>two</div>")
    assert("<div><p>Hello</p><div>one</div><div>two</div><p>There</p></div>" == TextUtil.stripNewlines(doc.body.html))
    doc.select("p").last.after("<p>Three</p><!-- four -->")
    assert("<div><p>Hello</p><div>one</div><div>two</div><p>There</p><p>Three</p><!-- four --></div>" == TextUtil.stripNewlines(doc.body.html))
  }

  test("testWrapWithRemainder") {
    val doc: Document = Jsoup.parse("<div><p>Hello</p></div>")
    val p: Element = doc.select("p").first()
    p.wrap("<div class='head'></div><p>There!</p>")
    assert("<div><div class=\"head\"><p>Hello</p><p>There!</p></div></div>" == TextUtil.stripNewlines(doc.body.html))
  }

  test("testHasText") {
    val doc: Document = Jsoup.parse("<div><p>Hello</p><p></p></div>")
    val div: Element = doc.select("div").first()
    val ps: Elements = doc.select("p")
    assert(div.hasText)
    assert(ps.first().hasText)
    assert(!ps.last.hasText)
  }

  test("dataset") {
    val doc: Document = Jsoup.parse("<div id=1 data-name=jsoup class=new data-package=jar>Hello</div><p id=2>Hello</p>")
    val div: Element = doc.select("div").first()
    val dataset: mutable.Map[String, String] = mutable.Map(div.dataset.toSeq: _*)
    val attributes: Attributes = div.attributes
    assert(2 == dataset.size)
    assert("jsoup" == dataset("name"))
    assert("jar" == dataset("package"))
    dataset.put("name", "jsoup updated")
    dataset.put("language", "java")
    dataset.remove("package")
    assert(2 == dataset.size)
    assert(4 == attributes.size)
    assert("jsoup updated" == attributes.get("data-name"))
    assert("jsoup updated" == dataset("name"))
    assert("java" == attributes.get("data-language"))
    assert("java" == dataset("language"))
    attributes.put("data-food", "bacon")
    assert(3 == dataset.size)
    assert("bacon" == dataset("food"))
    attributes.put("data-", "empty")
    assert(null == dataset(""))
    val p: Element = doc.select("p").first()
    assert(0 == p.dataset.size)
  }

  test("parentlessToString") {
    val doc: Document = Jsoup.parse("<img src='foo'>")
    val img: Element = doc.select("img").first()
    assert("<img src=\"foo\">" == img.toString)
    img.remove()
    assert("<img src=\"foo\">" == img.toString)
  }

  test("testClone") {
    val doc: Document = Jsoup.parse("<div><p>One<p><span>Two</div>")
    val p: Element = doc.select("p").get(1)
    val clone: Element = p.clone()
    assert(clone.parent == null)
    assert(0 == clone.siblingIndex)
    assert(1 == p.siblingIndex)
    assert(p.parent != null)
    clone.append("<span>Three")
    assert("<p><span>Two</span><span>Three</span></p>" == TextUtil.stripNewlines(clone.outerHtml))
    assert("<div><p>One</p><p><span>Two</span></p></div>" == TextUtil.stripNewlines(doc.body.html))
    doc.body.appendChild(clone)
    assert(clone.parent != null)
    assert("<div><p>One</p><p><span>Two</span></p></div><p><span>Two</span><span>Three</span></p>" == TextUtil.stripNewlines(doc.body.html))
  }

  test("testClonesClassnames") {
    val doc: Document = Jsoup.parse("<div class='one two'></div>")
    val div: Element = doc.select("div").first()
    val classes: Set[String] = div.classNames
    assert(2 == classes.size)
    assert(classes.contains("one"))
    assert(classes.contains("two"))
    val copy: Element = div.clone()
    val copyClasses: mutable.Set[String] = new mutable.HashSet[String]
    copyClasses ++= copy.classNames
    assert(2 == copyClasses.size)
    assert(copyClasses.contains("one"))
    assert(copyClasses.contains("two"))
    copyClasses.add("three")
    copyClasses.remove("one")
    assert(classes.contains("one"))
    assert(!classes.contains("three"))
    assert(!copyClasses.contains("one"))
    assert(copyClasses.contains("three"))
    assert("" == div.html)
    assert("" == copy.html)
  }

  test("testTagNameSet") {
    val doc: Document = Jsoup.parse("<div><i>Hello</i>")
    doc.select("i").first().tagName("em")
    assert(0 == doc.select("i").size)
    assert(1 == doc.select("em").size)
    assert("<em>Hello</em>" == doc.select("div").first().html)
  }

  test("testHtmlContainsOuter") {
    val doc: Document = Jsoup.parse("<title>Check</title> <div>Hello there</div>")
    doc.outputSettings.indentAmount(0)
    assert(doc.html.contains(doc.select("title").outerHtml))
    assert(doc.html.contains(doc.select("div").outerHtml))
  }

  test("testGetTextNodes") {
    val doc: Document = Jsoup.parse("<p>One <span>Two</span> Three <br> Four</p>")
    val textNodes: Seq[TextNode] = doc.select("p").first().textNodes
    assert(3 == textNodes.size)
    assert("One " == textNodes(0).text())
    assert(" Three " == textNodes(1).text())
    assert(" Four" == textNodes(2).text())
    assert(0 == doc.select("br").first().textNodes.size)
  }

  test("testManipulateTextNodes") {
    val doc: Document = Jsoup.parse("<p>One <span>Two</span> Three <br> Four</p>")
    val p: Element = doc.select("p").first()
    val textNodes: Seq[TextNode] = p.textNodes
    textNodes(1).text(" three-more ")
    textNodes(2).splitText(3).text("-ur")
    assert("One Two three-more Fo-ur" == p.text())
    assert("One three-more Fo-ur" == p.ownText)
    assert(4 == p.textNodes.size)
  }

  test("testGetDataNodes") {
    val doc: Document = Jsoup.parse("<script>One Two</script> <style>Three Four</style> <p>Fix Six</p>")
    val script: Element = doc.select("script").first()
    val style: Element = doc.select("style").first()
    val p: Element = doc.select("p").first()
    val scriptData: Seq[DataNode] = script.dataNodes
    assert(1 == scriptData.size)
    assert("One Two" == scriptData(0).getWholeData)
    val styleData: Seq[DataNode] = style.dataNodes
    assert(1 == styleData.size)
    assert("Three Four" == styleData(0).getWholeData)
    val pData: Seq[DataNode] = p.dataNodes
    assert(0 == pData.size)
  }

  test("elementIsNotASiblingOfItself") {
    val doc: Document = Jsoup.parse("<div><p>One<p>Two<p>Three</div>")
    val p2: Element = doc.select("p").get(1)
    assert("Two" == p2.text())
    val els: Elements = p2.siblingElements
    assert(2 == els.size)
    assert("<p>One</p>" == els.get(0).outerHtml)
    assert("<p>Three</p>" == els.get(1).outerHtml)
  }

  test("testChildThrowsIndexOutOfBoundsOnMissing") {
    val doc: Document = Jsoup.parse("<div><p>One</p><p>Two</p></div>")
    val div: Element = doc.select("div").first()
    assert(2 == div.children.size)
    assert("One" == div.child(0).text())
    try {
      div.child(3)
      fail("Should throw index out of bounds")
    }
    catch {
      case e: IndexOutOfBoundsException =>
    }
  }

  test("moveByAppend") {
    val doc: Document = Jsoup.parse("<div id=1>Text <p>One</p> Text <p>Two</p></div><div id=2></div>")
    val div1: Element = doc.select("div").get(0)
    val div2: Element = doc.select("div").get(1)
    assert(4 == div1.childNodeSize)
    val children: Seq[Node] = div1.getChildNodes
    assert(4 == children.size)
    div2.insertChildren(0, children)
    assert(0 == children.size)
    assert(0 == div1.childNodeSize)
    assert(4 == div2.childNodeSize)
    assert("<div id=\"1\"></div>\n<div id=\"2\">\n Text \n <p>One</p> Text \n <p>Two</p>\n</div>" == doc.body.html)
  }

  test("insertChildrenArgumentValidation") {
    val doc: Document = Jsoup.parse("<div id=1>Text <p>One</p> Text <p>Two</p></div><div id=2></div>")
    val div1: Element = doc.select("div").get(0)
    val div2: Element = doc.select("div").get(1)
    val children: Seq[Node] = div1.getChildNodes
    try {
      div2.insertChildren(6, children)
      fail()
    }
    catch {
      case e: IllegalArgumentException =>
    }
    try {
      div2.insertChildren(-5, children)
      fail()
    }
    catch {
      case e: IllegalArgumentException =>
    }
    try {
      div2.insertChildren(0, null)
      fail()
    }
    catch {
      case e: IllegalArgumentException =>
    }
  }

  test("insertChildrenAtPosition") {
    val doc: Document = Jsoup.parse("<div id=1>Text1 <p>One</p> Text2 <p>Two</p></div><div id=2>Text3 <p>Three</p></div>")
    val div1: Element = doc.select("div").get(0)
    val p1s: Elements = div1.select("p")
    val div2: Element = doc.select("div").get(1)
    assert(2 == div2.childNodeSize)
    div2.insertChildren(-1, p1s)
    assert(2 == div1.childNodeSize)
    assert(4 == div2.childNodeSize)
    assert(3 == p1s.get(1).siblingIndex)
    val els: mutable.Buffer[Node] = new mutable.ArrayBuffer[Node]
    val el1: Element = new Element(Tag("span"), "").text("Span1")
    val el2: Element = new Element(Tag("span"), "").text("Span2")
    val tn1: TextNode = new TextNode("Text4", "")
    els.append(el1)
    els.append(el2)
    els.append(tn1)
    assert(el1.parent == null)
    div2.insertChildren(-2, els)
    assert(div2 == el1.parent)
    assert(7 == div2.childNodeSize)
    assert(3 == el1.siblingIndex)
    assert(4 == el2.siblingIndex)
    assert(5 == tn1.siblingIndex)
  }

  test("insertChildrenAsCopy") {
    val doc: Document = Jsoup.parse("<div id=1>Text <p>One</p> Text <p>Two</p></div><div id=2></div>")
    val div1: Element = doc.select("div").get(0)
    val div2: Element = doc.select("div").get(1)
    val ps: Elements = doc.select("p").clone()
    ps.first().text("One cloned")
    div2.insertChildren(-1, ps)
    assert(4 == div1.childNodeSize)
    assert(2 == div2.childNodeSize)
    assert("<div id=\"1\">Text <p>One</p> Text <p>Two</p></div><div id=\"2\"><p>One cloned</p><p>Two</p></div>" == TextUtil.stripNewlines(doc.body.html))
  }

  test("testCssPath") {
    val doc: Document = Jsoup.parse("<div id=\"id1\">A</div><div>B</div><div class=\"c1 c2\">C</div>")
    val divA: Element = doc.select("div").get(0)
    val divB: Element = doc.select("div").get(1)
    val divC: Element = doc.select("div").get(2)
    assert(divA.cssSelector == "#id1")
    assert(divB.cssSelector == "html > body > div:nth-child(2)")
    assert(divC.cssSelector == "html > body > div.c1.c2")
    assert(divA eq doc.select(divA.cssSelector).first())
    assert(divB eq doc.select(divB.cssSelector).first())
    assert(divC eq doc.select(divC.cssSelector).first())
  }

  test("testClassNames") {
    val doc: Document = Jsoup.parse("<div class=\"c1 c2\">C</div>")
    val div: Element = doc.select("div").get(0)
    assert("c1 c2" == div.className)
    val set1: mutable.Set[String] = new mutable.HashSet[String]()
    set1 ++= div.classNames
    val arr1: Array[AnyRef] = set1.toArray
    assert(arr1.length == 2)
    assert("c1" == arr1(0))
    assert("c2" == arr1(1))
    set1 += "c3"
    assert(2 == div.classNames.size)
    assert("c1 c2" == div.className)
    val newSet: mutable.Set[String] = new mutable.LinkedHashSet[String]()
    newSet ++= set1
    newSet.add("c3")
    div.classNames(newSet.toSet)
    assert("c1 c2 c3" == div.className)
    val set2: Set[String] = div.classNames
    val arr2: Array[AnyRef] = set2.toArray
    assert(arr2.length == 3)
    assert("c1" == arr2(0))
    assert("c2" == arr2(1))
    assert("c3" == arr2(2))
  }
}
