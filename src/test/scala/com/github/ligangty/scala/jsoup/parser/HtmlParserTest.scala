package com.github.ligangty.scala.jsoup.parser

import com.github.ligangty.scala.jsoup.helper.Strings
import com.github.ligangty.scala.jsoup.{TextUtil, Jsoup}
import com.github.ligangty.scala.jsoup.nodes._
import com.github.ligangty.scala.jsoup.select.Elements
import org.scalatest.FunSuite

/**
 * Tests for the Parser
 */
class HtmlParserTest extends FunSuite {

  test("parsesSimpleDocument") {
    val html: String = "<html><head><title>First!</title></head><body><p>First post! <img src=\"foo.png\" /></p></body></html>"
    val doc: Document = Jsoup.parse(html)
    val p: Element = doc.body.child(0)
    assert("p" == p.tagName)
    val img: Element = p.child(0)
    assert("foo.png" == img.attr("src"))
    assert("img" == img.tagName)
  }

  test("parsesRoughAttributes") {
    val html: String = "<html><head><title>First!</title></head><body><p class=\"foo > bar\">First post! <img src=\"foo.png\" /></p></body></html>"
    val doc: Document = Jsoup.parse(html)
    val p: Element = doc.body.child(0)
    assert("p" == p.tagName)
    assert("foo > bar" == p.attr("class"))
  }

  test("parsesQuiteRoughAttributes") {
    val html: String = "<p =a>One<a <p>Something</p>Else"
    var doc: Document = Jsoup.parse(html)
    assert("<p =a=\"\">One<a <p=\"\">Something</a></p>\n" + "<a <p=\"\">Else</a>" == doc.body.html)
    doc = Jsoup.parse("<p .....>")
    assert("<p .....=\"\"></p>" == doc.body.html)
  }

  test("parsesComments") {
    val html: String = "<html><head></head><body><img src=foo><!-- <table><tr><td></table> --><p>Hello</p></body></html>"
    val doc: Document = Jsoup.parse(html)
    val body: Element = doc.body
    val comment: Comment = body.getChildNode(1).asInstanceOf[Comment]
    assert(" <table><tr><td></table> " == comment.getData)
    val p: Element = body.child(1)
    val text: TextNode = p.getChildNode(0).asInstanceOf[TextNode]
    assert("Hello" == text.getWholeText)
  }

  test("parsesUnterminatedComments") {
    val html: String = "<p>Hello<!-- <tr><td>"
    val doc: Document = Jsoup.parse(html)
    val p: Element = doc.getElementsByTag("p").get(0)
    assert("Hello" == p.text)
    val text: TextNode = p.getChildNode(0).asInstanceOf[TextNode]
    assert("Hello" == text.getWholeText)
    val comment: Comment = p.getChildNode(1).asInstanceOf[Comment]
    assert(" <tr><td>" == comment.getData)
  }

  test("dropsUnterminatedTag") {
    val h1: String = "<p"
    var doc: Document = Jsoup.parse(h1)
    assert(0 == doc.getElementsByTag("p").size)
    assert("" == doc.text())
    val h2: String = "<div id=1<p id='2'"
    doc = Jsoup.parse(h2)
    assert("" == doc.text())
  }

  test("dropsUnterminatedAttribute") {
    val h1: String = "<p id=\"foo"
    val doc: Document = Jsoup.parse(h1)
    assert("" == doc.text())
  }

  test("parsesUnterminatedTextarea") {
    val doc: Document = Jsoup.parse("<body><p><textarea>one<p>two")
    val t: Element = doc.select("textarea").first()
    assert("one" == t.text)
    assert("two" == doc.select("p").get(1).text)
  }

  test("parsesUnterminatedOption") {
    val doc: Document = Jsoup.parse("<body><p><select><option>One<option>Two</p><p>Three</p>")
    val options: Elements = doc.select("option")
    assert(2 == options.size)
    assert("One" == options.first().text)
    assert("TwoThree" == options.last.text)
  }

  test("testSpaceAfterTag") {
    val doc: Document = Jsoup.parse("<div > <a name=\"top\"></a ><p id=1 >Hello</p></div>")
    assert("<div> <a name=\"top\"></a><p id=\"1\">Hello</p></div>" == TextUtil.stripNewlines(doc.body.html))
  }

  test("createsDocumentStructure") {
    val html: String = "<meta name=keywords /><link rel=stylesheet /><title>jsoup</title><p>Hello world</p>"
    val doc: Document = Jsoup.parse(html)
    val head: Element = doc.head
    val body: Element = doc.body
    assert(1 == doc.children.size)
    assert(2 == doc.child(0).children.size)
    assert(3 == head.children.size)
    assert(1 == body.children.size)
    assert("keywords" == head.getElementsByTag("meta").get(0).attr("name"))
    assert(0 == body.getElementsByTag("meta").size)
    assert("jsoup" == doc.title)
    assert("Hello world" == body.text)
    assert("Hello world" == body.children.get(0).text)
  }

  test("createsStructureFromBodySnippet") {
    val html: String = "foo <b>bar</b> baz"
    val doc: Document = Jsoup.parse(html)
    assert("foo bar baz" == doc.text())
  }

  test("handlesEscapedData") {
    val html: String = "<div title='Surf &amp; Turf'>Reef &amp; Beef</div>"
    val doc: Document = Jsoup.parse(html)
    val div: Element = doc.getElementsByTag("div").get(0)
    assert("Surf & Turf" == div.attr("title"))
    assert("Reef & Beef" == div.text)
  }

  test("handlesDataOnlyTags") {
    val t: String = "<style>font-family: bold</style>"
    val tels: Seq[Element] = Jsoup.parse(t).getElementsByTag("style").toSeq
    assert("font-family: bold" == tels.head.data)
    assert("" == tels.head.text)
    val s: String = "<p>Hello</p><script>obj.insert('<a rel=\"none\" />');\ni++;</script><p>There</p>"
    val doc: Document = Jsoup.parse(s)
    assert("Hello There" == doc.text())
    assert("obj.insert('<a rel=\"none\" />');\ni++;" == doc.data)
  }

  test("handlesTextAfterData") {
    val h: String = "<html><body>pre <script>inner</script> aft</body></html>"
    val doc: Document = Jsoup.parse(h)
    assert("<html><head></head><body>pre <script>inner</script> aft</body></html>" == TextUtil.stripNewlines(doc.html))
  }

  test("handlesTextArea") {
    val doc: Document = Jsoup.parse("<textarea>Hello</textarea>")
    val els: Elements = doc.select("textarea")
    assert("Hello" == els.text)
    assert("Hello" == els.value)
  }

  test("preservesSpaceInTextArea") {
    val doc: Document = Jsoup.parse("<textarea>\n\tOne\n\tTwo\n\tThree\n</textarea>")
    val expect: String = "One\n\tTwo\n\tThree"
    val el: Element = doc.select("textarea").first()
    assert(expect == el.text)
    assert(expect == el.value)
    assert(expect == el.html)
    assert("<textarea>\n\t" + expect + "\n</textarea>" == el.outerHtml)
  }

  test("preservesSpaceInScript") {
    val doc: Document = Jsoup.parse("<script>\nOne\n\tTwo\n\tThree\n</script>")
    val expect: String = "\nOne\n\tTwo\n\tThree\n"
    val el: Element = doc.select("script").first()
    assert(expect == el.data)
    assert("One\n\tTwo\n\tThree" == el.html)
    assert("<script>" + expect + "</script>" == el.outerHtml)
  }

  test("doesNotCreateImplicitLists") {
    val h: String = "<li>Point one<li>Point two"
    val doc: Document = Jsoup.parse(h)
    val ol: Elements = doc.select("ul")
    assert(0 == ol.size)
    val lis: Elements = doc.select("li")
    assert(2 == lis.size)
    assert("body" == lis.first().parent.tagName)
    val h2: String = "<ol><li><p>Point the first<li><p>Point the second"
    val doc2: Document = Jsoup.parse(h2)
    assert(0 == doc2.select("ul").size)
    assert(1 == doc2.select("ol").size)
    assert(2 == doc2.select("ol li").size)
    assert(2 == doc2.select("ol li p").size)
    assert(1 == doc2.select("ol li").get(0).children.size)
  }

  test("discardsNakedTds") {
    val h: String = "<td>Hello<td><p>There<p>now"
    val doc: Document = Jsoup.parse(h)
    assert("Hello<p>There</p><p>now</p>" == TextUtil.stripNewlines(doc.body.html))
  }

  test("handlesNestedImplicitTable") {
    val doc: Document = Jsoup.parse("<table><td>1</td></tr> <td>2</td></tr> <td> <table><td>3</td> <td>4</td></table> <tr><td>5</table>")
    assert("<table><tbody><tr><td>1</td></tr> <tr><td>2</td></tr> <tr><td> <table><tbody><tr><td>3</td> <td>4</td></tr></tbody></table> </td></tr><tr><td>5</td></tr></tbody></table>" == TextUtil.stripNewlines(doc.body.html))
  }

  test("handlesWhatWgExpensesTableExample") {
    val doc: Document = Jsoup.parse("<table> <colgroup> <col> <colgroup> <col> <col> <col> <thead> <tr> <th> <th>2008 <th>2007 <th>2006 <tbody> <tr> <th scope=rowgroup> Research and development <td> $ 1,109 <td> $ 782 <td> $ 712 <tr> <th scope=row> Percentage of net sales <td> 3.4% <td> 3.3% <td> 3.7% <tbody> <tr> <th scope=rowgroup> Selling, general, and administrative <td> $ 3,761 <td> $ 2,963 <td> $ 2,433 <tr> <th scope=row> Percentage of net sales <td> 11.6% <td> 12.3% <td> 12.6% </table>")
    assert("<table> <colgroup> <col> </colgroup><colgroup> <col> <col> <col> </colgroup><thead> <tr> <th> </th><th>2008 </th><th>2007 </th><th>2006 </th></tr></thead><tbody> <tr> <th scope=\"rowgroup\"> Research and development </th><td> $ 1,109 </td><td> $ 782 </td><td> $ 712 </td></tr><tr> <th scope=\"row\"> Percentage of net sales </th><td> 3.4% </td><td> 3.3% </td><td> 3.7% </td></tr></tbody><tbody> <tr> <th scope=\"rowgroup\"> Selling, general, and administrative </th><td> $ 3,761 </td><td> $ 2,963 </td><td> $ 2,433 </td></tr><tr> <th scope=\"row\"> Percentage of net sales </th><td> 11.6% </td><td> 12.3% </td><td> 12.6% </td></tr></tbody></table>" == TextUtil.stripNewlines(doc.body.html))
  }

  test("handlesTbodyTable") {
    val doc: Document = Jsoup.parse("<html><head></head><body><table><tbody><tr><td>aaa</td><td>bbb</td></tr></tbody></table></body></html>")
    assert("<table><tbody><tr><td>aaa</td><td>bbb</td></tr></tbody></table>" == TextUtil.stripNewlines(doc.body.html))
  }

  test("handlesImplicitCaptionClose") {
    val doc: Document = Jsoup.parse("<table><caption>A caption<td>One<td>Two")
    assert("<table><caption>A caption</caption><tbody><tr><td>One</td><td>Two</td></tr></tbody></table>" == TextUtil.stripNewlines(doc.body.html))
  }

  test("noTableDirectInTable") {
    val doc: Document = Jsoup.parse("<table> <td>One <td><table><td>Two</table> <table><td>Three")
    assert("<table> <tbody><tr><td>One </td><td><table><tbody><tr><td>Two</td></tr></tbody></table> <table><tbody><tr><td>Three</td></tr></tbody></table></td></tr></tbody></table>" == TextUtil.stripNewlines(doc.body.html))
  }

  test("ignoresDupeEndTrTag") {
    val doc: Document = Jsoup.parse("<table><tr><td>One</td><td><table><tr><td>Two</td></tr></tr></table></td><td>Three</td></tr></table>")
    assert("<table><tbody><tr><td>One</td><td><table><tbody><tr><td>Two</td></tr></tbody></table></td><td>Three</td></tr></tbody></table>" == TextUtil.stripNewlines(doc.body.html))
  }

  test("handlesBaseTags") {
    val h: String = "<a href=1>#</a><base href='/2/'><a href='3'>#</a><base href='http://bar'><a href=/4>#</a>"
    val doc: Document = Jsoup.parse(h, "http://foo/")
    assert("http://foo/2/" == doc.baseUri)
    val anchors: Elements = doc.getElementsByTag("a")
    assert(3 == anchors.size)
    assert("http://foo/2/" == anchors.get(0).baseUri)
    assert("http://foo/2/" == anchors.get(1).baseUri)
    assert("http://foo/2/" == anchors.get(2).baseUri)
    assert("http://foo/2/1" == anchors.get(0).absUrl("href"))
    assert("http://foo/2/3" == anchors.get(1).absUrl("href"))
    assert("http://foo/4" == anchors.get(2).absUrl("href"))
  }

  test("handlesProtocolRelativeUrl") {
    val base: String = "https://example.com/"
    val html: String = "<img src='//example.net/img.jpg'>"
    val doc: Document = Jsoup.parse(html, base)
    val el: Element = doc.select("img").first()
    assert("https://example.net/img.jpg" == el.absUrl("src"))
  }

  test("handlesCdata") {
    val h: String = "<div id=1><![CDATA[<html>\n<foo><&amp;]]></div>"
    val doc: Document = Jsoup.parse(h)
    val div: Element = doc.getElementById("1")
    assert("<html> <foo><&amp;" == div.text)
    assert(0 == div.children.size)
    assert(1 == div.childNodeSize)
  }

  test("handlesUnclosedCdataAtEOF") {
    val h: String = "<![CDATA[]]"
    val doc: Document = Jsoup.parse(h)
    assert(1 == doc.body.childNodeSize)
  }

  test("handlesInvalidStartTags") {
    val h: String = "<div>Hello < There <&amp;></div>"
    val doc: Document = Jsoup.parse(h)
    assert("Hello < There <&>" == doc.select("div").first().text)
  }

  test("handlesUnknownTags") {
    val h: String = "<div><foo title=bar>Hello<foo title=qux>there</foo></div>"
    val doc: Document = Jsoup.parse(h)
    val foos: Elements = doc.select("foo")
    assert(2 == foos.size)
    assert("bar" == foos.first().attr("title"))
    assert("qux" == foos.last.attr("title"))
    assert("there" == foos.last.text)
  }

  test("handlesUnknownInlineTags") {
    val h: String = "<p><cust>Test</cust></p><p><cust><cust>Test</cust></cust></p>"
    val doc: Document = Jsoup.parseBodyFragment(h)
    val out: String = doc.body.html
    assert(h == TextUtil.stripNewlines(out))
  }

  test("parsesBodyFragment") {
    val h: String = "<!-- comment --><p><a href='foo'>One</a></p>"
    val doc: Document = Jsoup.parseBodyFragment(h, "http://example.com")
    assert("<body><!-- comment --><p><a href=\"foo\">One</a></p></body>" == TextUtil.stripNewlines(doc.body.outerHtml))
    assert("http://example.com/foo" == doc.select("a").first().absUrl("href"))
  }

  test("handlesUnknownNamespaceTags") {
    val h: String = "<foo:bar id='1' /><abc:def id=2>Foo<p>Hello</p></abc:def><foo:bar>There</foo:bar>"
    val doc: Document = Jsoup.parse(h)
    assert("<foo:bar id=\"1\" /><abc:def id=\"2\">Foo<p>Hello</p></abc:def><foo:bar>There</foo:bar>" == TextUtil.stripNewlines(doc.body.html))
  }

  test("handlesKnownEmptyBlocks") {
    val h: String = "<div id='1' /><script src='/foo' /><div id=2><img /><img></div><a id=3 /><i /><foo /><foo>One</foo> <hr /> hr text <hr> hr text two"
    val doc: Document = Jsoup.parse(h)
    assert("<div id=\"1\"></div><script src=\"/foo\"></script><div id=\"2\"><img><img></div><a id=\"3\"></a><i></i><foo /><foo>One</foo> <hr> hr text <hr> hr text two" == TextUtil.stripNewlines(doc.body.html))
  }

  test("handlesSolidusAtAttributeEnd") {
    val h: String = "<a href=/>link</a>"
    val doc: Document = Jsoup.parse(h)
    assert("<a href=\"/\">link</a>" == doc.body.html)
  }

  test("handlesMultiClosingBody") {
    val h: String = "<body><p>Hello</body><p>there</p></body></body></html><p>now"
    val doc: Document = Jsoup.parse(h)
    assert(3 == doc.select("p").size)
    assert(3 == doc.body.children.size)
  }

  test("handlesUnclosedDefinitionLists") {
    val h: String = "<dt>Foo<dd>Bar<dt>Qux<dd>Zug"
    val doc: Document = Jsoup.parse(h)
    assert(0 == doc.select("dl").size)
    assert(4 == doc.select("dt, dd").size)
    val dts: Elements = doc.select("dt")
    assert(2 == dts.size)
    assert("Zug" == dts.get(1).nextElementSibling.text)
  }

  test("handlesBlocksInDefinitions") {
    val h: String = "<dl><dt><div id=1>Term</div></dt><dd><div id=2>Def</div></dd></dl>"
    val doc: Document = Jsoup.parse(h)
    assert("dt" == doc.select("#1").first().parent.tagName)
    assert("dd" == doc.select("#2").first().parent.tagName)
    assert("<dl><dt><div id=\"1\">Term</div></dt><dd><div id=\"2\">Def</div></dd></dl>" == TextUtil.stripNewlines(doc.body.html))
  }

  test("handlesFrames") {
    val h: String = "<html><head><script></script><noscript></noscript></head><frameset><frame src=foo></frame><frame src=foo></frameset></html>"
    val doc: Document = Jsoup.parse(h)
    assert("<html><head><script></script><noscript></noscript></head><frameset><frame src=\"foo\"><frame src=\"foo\"></frameset></html>" == TextUtil.stripNewlines(doc.html))
  }

  test("ignoresContentAfterFrameset") {
    val h: String = "<html><head><title>One</title></head><frameset><frame /><frame /></frameset><table></table></html>"
    val doc: Document = Jsoup.parse(h)
    assert("<html><head><title>One</title></head><frameset><frame><frame></frameset></html>" == TextUtil.stripNewlines(doc.html))
  }

  test("handlesJavadocFont") {
    val h: String = "<TD BGCOLOR=\"#EEEEFF\" CLASS=\"NavBarCell1\">    <A HREF=\"deprecated-list.html\"><FONT CLASS=\"NavBarFont1\"><B>Deprecated</B></FONT></A>&nbsp;</TD>"
    val doc: Document = Jsoup.parse(h)
    val a: Element = doc.select("a").first()
    assert("Deprecated" == a.text)
    assert("font" == a.child(0).tagName)
    assert("b" == a.child(0).child(0).tagName)
  }

  test("handlesBaseWithoutHref") {
    val h: String = "<head><base target='_blank'></head><body><a href=/foo>Test</a></body>"
    val doc: Document = Jsoup.parse(h, "http://example.com/")
    val a: Element = doc.select("a").first()
    assert("/foo" == a.attr("href"))
    assert("http://example.com/foo" == a.attr("abs:href"))
  }

  test("normalisesDocument") {
    val h: String = "<!doctype html>One<html>Two<head>Three<link></head>Four<body>Five </body>Six </html>Seven "
    val doc: Document = Jsoup.parse(h)
    assert("<!doctype html><html><head></head><body>OneTwoThree<link>FourFive Six Seven </body></html>" == TextUtil.stripNewlines(doc.html))
  }

  test("normalisesEmptyDocument") {
    val doc: Document = Jsoup.parse("")
    assert("<html><head></head><body></body></html>" == TextUtil.stripNewlines(doc.html))
  }

  test("normalisesHeadlessBody") {
    val doc: Document = Jsoup.parse("<html><body><span class=\"foo\">bar</span>")
    assert("<html><head></head><body><span class=\"foo\">bar</span></body></html>" == TextUtil.stripNewlines(doc.html))
  }

  test("normalisedBodyAfterContent") {
    val doc: Document = Jsoup.parse("<font face=Arial><body class=name><div>One</div></body></font>")
    assert("<html><head></head><body class=\"name\"><font face=\"Arial\"><div>One</div></font></body></html>" == TextUtil.stripNewlines(doc.html))
  }

  test("findsCharsetInMalformedMeta") {
    val h: String = "<meta http-equiv=Content-Type content=text/html; charset=gb2312>"
    val doc: Document = Jsoup.parse(h)
    assert("gb2312" == doc.select("meta").attr("charset"))
  }

  test("testHgroup") {
    val doc: Document = Jsoup.parse("<h1>Hello <h2>There <hgroup><h1>Another<h2>headline</hgroup> <hgroup><h1>More</h1><p>stuff</p></hgroup>")
    assert("<h1>Hello </h1><h2>There <hgroup><h1>Another</h1><h2>headline</h2></hgroup> <hgroup><h1>More</h1><p>stuff</p></hgroup></h2>" == TextUtil.stripNewlines(doc.body.html))
  }

  test("testRelaxedTags") {
    val doc: Document = Jsoup.parse("<abc_def id=1>Hello</abc_def> <abc-def>There</abc-def>")
    assert("<abc_def id=\"1\">Hello</abc_def> <abc-def>There</abc-def>" == TextUtil.stripNewlines(doc.body.html))
  }

  test("testHeaderContents") {
    val doc: Document = Jsoup.parse("<h1>Hello <div>There</div> now</h1> <h2>More <h3>Content</h3></h2>")
    assert("<h1>Hello <div>There</div> now</h1> <h2>More </h2><h3>Content</h3>" == TextUtil.stripNewlines(doc.body.html))
  }

  test("testSpanContents") {
    val doc: Document = Jsoup.parse("<span>Hello <div>there</div> <span>now</span></span>")
    assert("<span>Hello <div>there</div> <span>now</span></span>" == TextUtil.stripNewlines(doc.body.html))
  }

  test("testNoImagesInNoScriptInHead") {
    val doc: Document = Jsoup.parse("<html><head><noscript><img src='foo'></noscript></head><body><p>Hello</p></body></html>")
    assert("<html><head><noscript>&lt;img src=\"foo\"&gt;</noscript></head><body><p>Hello</p></body></html>" == TextUtil.stripNewlines(doc.html))
  }

  test("testAFlowContents") {
    val doc: Document = Jsoup.parse("<a>Hello <div>there</div> <span>now</span></a>")
    assert("<a>Hello <div>there</div> <span>now</span></a>" == TextUtil.stripNewlines(doc.body.html))
  }

  test("testFontFlowContents") {
    val doc: Document = Jsoup.parse("<font>Hello <div>there</div> <span>now</span></font>")
    assert("<font>Hello <div>there</div> <span>now</span></font>" == TextUtil.stripNewlines(doc.body.html))
  }

  test("handlesMisnestedTagsBI") {
    val h: String = "<p>1<b>2<i>3</b>4</i>5</p>"
    val doc: Document = Jsoup.parse(h)
    assert("<p>1<b>2<i>3</i></b><i>4</i>5</p>" == doc.body.html)
  }

  test("handlesMisnestedTagsBP") {
    val h: String = "<b>1<p>2</b>3</p>"
    val doc: Document = Jsoup.parse(h)
    assert("<b>1</b>\n<p><b>2</b>3</p>" == doc.body.html)
  }

  test("handlesUnexpectedMarkupInTables") {
    val h: String = "<table><b><tr><td>aaa</td></tr>bbb</table>ccc"
    val doc: Document = Jsoup.parse(h)
    assert("<b></b><b>bbb</b><table><tbody><tr><td>aaa</td></tr></tbody></table><b>ccc</b>" == TextUtil.stripNewlines(doc.body.html))
  }

  test("handlesUnclosedFormattingElements") {
    val h: String = "<!DOCTYPE html>\n" + "<p><b class=x><b class=x><b><b class=x><b class=x><b>X\n" + "<p>X\n" + "<p><b><b class=x><b>X\n" + "<p></b></b></b></b></b></b>X"
    val doc: Document = Jsoup.parse(h)
    doc.outputSettings.indentAmount(0)
    val want: String = "<!doctype html>\n" + "<html>\n" + "<head></head>\n" + "<body>\n" + "<p><b class=\"x\"><b class=\"x\"><b><b class=\"x\"><b class=\"x\"><b>X </b></b></b></b></b></b></p>\n" + "<p><b class=\"x\"><b><b class=\"x\"><b class=\"x\"><b>X </b></b></b></b></b></p>\n" + "<p><b class=\"x\"><b><b class=\"x\"><b class=\"x\"><b><b><b class=\"x\"><b>X </b></b></b></b></b></b></b></b></p>\n" + "<p>X</p>\n" + "</body>\n" + "</html>"
    assert(want == doc.html)
  }

  test("handlesUnclosedAnchors") {
    val h: String = "<a href='http://example.com/'>Link<p>Error link</a>"
    val doc: Document = Jsoup.parse(h)
    val want: String = "<a href=\"http://example.com/\">Link</a>\n<p><a href=\"http://example.com/\">Error link</a></p>"
    assert(want == doc.body.html)
  }

  test("reconstructFormattingElements") {
    val h: String = "<p><b class=one>One <i>Two <b>Three</p><p>Hello</p>"
    val doc: Document = Jsoup.parse(h)
    assert("<p><b class=\"one\">One <i>Two <b>Three</b></i></b></p>\n<p><b class=\"one\"><i><b>Hello</b></i></b></p>" == doc.body.html)
  }

  test("reconstructFormattingElementsInTable") {
    val h: String = "<p><b>One</p> <table><tr><td><p><i>Three<p>Four</i></td></tr></table> <p>Five</p>"
    val doc: Document = Jsoup.parse(h)
    val want: String = "<p><b>One</b></p>\n" + "<b> \n" + " <table>\n" + "  <tbody>\n" + "   <tr>\n" + "    <td><p><i>Three</i></p><p><i>Four</i></p></td>\n" + "   </tr>\n" + "  </tbody>\n" + " </table> <p>Five</p></b>"
    assert(want == doc.body.html)
  }

  test("commentBeforeHtml") {
    val h: String = "<!-- comment --><!-- comment 2 --><p>One</p>"
    val doc: Document = Jsoup.parse(h)
    assert("<!-- comment --><!-- comment 2 --><html><head></head><body><p>One</p></body></html>" == TextUtil.stripNewlines(doc.html))
  }

  test("emptyTdTag") {
    val h: String = "<table><tr><td>One</td><td id='2' /></tr></table>"
    val doc: Document = Jsoup.parse(h)
    assert("<td>One</td>\n<td id=\"2\"></td>" == doc.select("tr").first().html)
  }

  test("handlesSolidusInA") {
    val h: String = "<a class=lp href=/lib/14160711/>link text</a>"
    val doc: Document = Jsoup.parse(h)
    val a: Element = doc.select("a").first()
    assert("link text" == a.text)
    assert("/lib/14160711/" == a.attr("href"))
  }

  test("handlesSpanInTbody") {
    val h: String = "<table><tbody><span class='1'><tr><td>One</td></tr><tr><td>Two</td></tr></span></tbody></table>"
    val doc: Document = Jsoup.parse(h)
    assert(doc.select("span").first().children.size == 0)
    assert(doc.select("table").size == 1)
  }

  test("handlesUnclosedTitleAtEof") {
    assert("Data" == Jsoup.parse("<title>Data").title)
    assert("Data<" == Jsoup.parse("<title>Data<").title)
    assert("Data</" == Jsoup.parse("<title>Data</").title)
    assert("Data</t" == Jsoup.parse("<title>Data</t").title)
    assert("Data</ti" == Jsoup.parse("<title>Data</ti").title)
    assert("Data" == Jsoup.parse("<title>Data</title>").title)
    assert("Data" == Jsoup.parse("<title>Data</title >").title)
  }

  test("handlesUnclosedTitle") {
    val one: Document = Jsoup.parse("<title>One <b>Two <b>Three</TITLE><p>Test</p>")
    assert("One <b>Two <b>Three" == one.title)
    assert("Test" == one.select("p").first().text)
    val two: Document = Jsoup.parse("<title>One<b>Two <p>Test</p>")
    assert("One" == two.title)
    assert("<b>Two <p>Test</p></b>" == two.body.html)
  }

  test("handlesUnclosedScriptAtEof") {
    assert("Data" == Jsoup.parse("<script>Data").select("script").first().data)
    assert("Data<" == Jsoup.parse("<script>Data<").select("script").first().data)
    assert("Data</sc" == Jsoup.parse("<script>Data</sc").select("script").first().data)
    assert("Data</-sc" == Jsoup.parse("<script>Data</-sc").select("script").first().data)
    assert("Data</sc-" == Jsoup.parse("<script>Data</sc-").select("script").first().data)
    assert("Data</sc--" == Jsoup.parse("<script>Data</sc--").select("script").first().data)
    assert("Data" == Jsoup.parse("<script>Data</script>").select("script").first().data)
    assert("Data</script" == Jsoup.parse("<script>Data</script").select("script").first().data)
    assert("Data" == Jsoup.parse("<script>Data</script ").select("script").first().data)
    assert("Data" == Jsoup.parse("<script>Data</script n").select("script").first().data)
    assert("Data" == Jsoup.parse("<script>Data</script n=").select("script").first().data)
    assert("Data" == Jsoup.parse("<script>Data</script n=\"").select("script").first().data)
    assert("Data" == Jsoup.parse("<script>Data</script n=\"p").select("script").first().data)
  }

  test("handlesUnclosedRawtextAtEof") {
    assert("Data" == Jsoup.parse("<style>Data").select("style").first().data)
    assert("Data</st" == Jsoup.parse("<style>Data</st").select("style").first().data)
    assert("Data" == Jsoup.parse("<style>Data</style>").select("style").first().data)
    assert("Data</style" == Jsoup.parse("<style>Data</style").select("style").first().data)
    assert("Data</-style" == Jsoup.parse("<style>Data</-style").select("style").first().data)
    assert("Data</style-" == Jsoup.parse("<style>Data</style-").select("style").first().data)
    assert("Data</style--" == Jsoup.parse("<style>Data</style--").select("style").first().data)
  }

  test("noImplicitFormForTextAreas") {
    val doc: Document = Jsoup.parse("<textarea>One</textarea>")
    assert("<textarea>One</textarea>" == doc.body.html)
  }

  test("handlesEscapedScript") {
    val doc: Document = Jsoup.parse("<script><!-- one <script>Blah</script> --></script>")
    assert("<!-- one <script>Blah</script> -->" == doc.select("script").first().data)
  }

  test("handles0CharacterAsText") {
    val doc: Document = Jsoup.parse("0<p>0</p>")
    assert("0\n<p>0</p>" == doc.body.html)
  }

  test("handlesNullInData") {
    val doc: Document = Jsoup.parse("<p id=\u0000>Blah \u0000</p>")
    assert("<p id=\"\uFFFD\">Blah \u0000</p>" == doc.body.html)
  }

  test("handlesNullInComments") {
    val doc: Document = Jsoup.parse("<body><!-- \u0000 \u0000 -->")
    assert("<!-- \uFFFD \uFFFD -->" == doc.body.html)
  }

  test("handlesNewlinesAndWhitespaceInTag") {
    val doc: Document = Jsoup.parse("<a \n href=\"one\" \r\n id=\"two\" \f >")
    assert("<a href=\"one\" id=\"two\"></a>" == doc.body.html)
  }

  test("handlesWhitespaceInoDocType") {
    val html: String = "<!DOCTYPE html\r\n" + "      PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\"\r\n" + "      \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">"
    val doc: Document = Jsoup.parse(html)
    assert("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">" == doc.getChildNode(0).outerHtml)
  }

  test("tracksErrorsWhenRequested") {
    val html: String = "<p>One</p href='no'><!DOCTYPE html>&arrgh;<font /><br /><foo"
    val parser: Parser = Parser.htmlParser.setTrackErrors(500)
    Jsoup.parse(html, "http://example.com", parser)
    val errors: Seq[ParseError] = parser.getErrors
    assert(5 == errors.size)
    assert("20: Attributes incorrectly present on end tag" == errors.head.toString)
    assert("35: Unexpected token [Doctype] when in state [InBody]" == errors(1).toString)
    assert("36: Invalid character reference: invalid named referenece 'arrgh'" == errors(2).toString)
    assert("50: Self closing flag not acknowledged" == errors(3).toString)
    assert("61: Unexpectedly reached end of file (EOF) in input state [TagName]" == errors(4).toString)
  }

  test("tracksLimitedErrorsWhenRequested") {
    val html: String = "<p>One</p href='no'><!DOCTYPE html>&arrgh;<font /><br /><foo"
    val parser: Parser = Parser.htmlParser.setTrackErrors(3)
    parser.parseInput(html, "http://example.com")
    val errors: Seq[ParseError] = parser.getErrors
    assert(3 == errors.size)
    assert("20: Attributes incorrectly present on end tag" == errors.head.toString)
    assert("35: Unexpected token [Doctype] when in state [InBody]" == errors(1).toString)
    assert("36: Invalid character reference: invalid named referenece 'arrgh'" == errors(2).toString)
  }

  test("noErrorsByDefault") {
    val html: String = "<p>One</p href='no'>&arrgh;<font /><br /><foo"
    val parser: Parser = Parser.htmlParser
    Jsoup.parse(html, "http://example.com", parser)
    val errors: Seq[ParseError] = parser.getErrors
    assert(0 == errors.size)
  }

  test("handlesCommentsInTable") {
    val html: String = "<table><tr><td>text</td><!-- Comment --></tr></table>"
    val node: Document = Jsoup.parseBodyFragment(html)
    assert("<html><head></head><body><table><tbody><tr><td>text</td><!-- Comment --></tr></tbody></table></body></html>" == TextUtil.stripNewlines(node.outerHtml))
  }

  test("handlesQuotesInCommentsInScripts") {
    val html: String = "<script>\n" + "  <!--\n" + "    document.write('</scr' + 'ipt>');\n" + "  // -->\n" + "</script>"
    val node: Document = Jsoup.parseBodyFragment(html)
    assert("<script>\n" + "  <!--\n" + "    document.write('</scr' + 'ipt>');\n" + "  // -->\n" + "</script>" == node.body.html)
  }

  test("handleNullContextInParseFragment") {
    val html: String = "<ol><li>One</li></ol><p>Two</p>"
    val nodes: Seq[Node] = Parser.parseFragment(html, null, "http://example.com/")
    assert(1 == nodes.size)
    assert("html" == nodes.head.nodeName)
    assert("<html> <head></head> <body> <ol> <li>One</li> </ol> <p>Two</p> </body> </html>" == Strings.normaliseWhitespace(nodes.head.outerHtml))
  }

  test("doesNotFindShortestMatchingEntity") {
    val html: String = "One &clubsuite; &clubsuit;"
    val doc: Document = Jsoup.parse(html)
    assert(Strings.normaliseWhitespace("One &amp;clubsuite; ♣") == doc.body.html)
  }

  test("relaxedBaseEntityMatchAndStrictExtendedMatch") {
    val html: String = "&amp &quot &reg &icy &hopf &icy; &hopf;"
    val doc: Document = Jsoup.parse(html)
    doc.outputSettings.escapeMode(Entities.EXTENDED).charset("ascii")
    assert("&amp; \" &reg; &amp;icy &amp;hopf &icy; &hopf;" == doc.body.html)
  }

  test("handlesXmlDeclarationAsBogusComment") {
    val html: String = "<?xml encoding='UTF-8' ?><body>One</body>"
    val doc: Document = Jsoup.parse(html)
    assert("<!--?xml encoding='UTF-8' ?--> <html> <head></head> <body> One </body> </html>" == Strings.normaliseWhitespace(doc.outerHtml))
  }

  test("handlesTagsInTextarea") {
    val html: String = "<textarea><p>Jsoup</p></textarea>"
    val doc: Document = Jsoup.parse(html)
    assert("<textarea>&lt;p&gt;Jsoup&lt;/p&gt;</textarea>" == doc.body.html)
  }

  test("createsFormElements") {
    val html: String = "<body><form><input id=1><input id=2></form></body>"
    val doc: Document = Jsoup.parse(html)
    val el: Element = doc.select("form").first()
    assert(el.isInstanceOf[FormElement], "Is form element")
    val form: FormElement = el.asInstanceOf[FormElement]
    val controls: Elements = form.elements
    assert(2 == controls.size)
    assert("1" == controls.get(0).id)
    assert("2" == controls.get(1).id)
  }

  test("associatedFormControlsWithDisjointForms") {
    val html: String = "<table><tr><form><input type=hidden id=1><td><input type=text id=2></td><tr></table>"
    val doc: Document = Jsoup.parse(html)
    val el: Element = doc.select("form").first()
    assert(el.isInstanceOf[FormElement], "Is form element")
    val form: FormElement = el.asInstanceOf[FormElement]
    val controls: Elements = form.elements
    assert(2 == controls.size)
    assert("1" == controls.get(0).id)
    assert("2" == controls.get(1).id)
    assert("<table><tbody><tr><form></form><input type=\"hidden\" id=\"1\"><td><input type=\"text\" id=\"2\"></td></tr><tr></tr></tbody></table>" == TextUtil.stripNewlines(doc.body.html))
  }

  test("handlesInputInTable") {
    val h: String = "<body>\n" + "<input type=\"hidden\" name=\"a\" value=\"\">\n" + "<table>\n" + "<input type=\"hidden\" name=\"b\" value=\"\" />\n" + "</table>\n" + "</body>"
    val doc: Document = Jsoup.parse(h)
    assert(1 == doc.select("table input").size)
    assert(2 == doc.select("input").size)
  }

  test("convertsImageToImg") {
    val h: String = "<body><image><svg><image /></svg></body>"
    val doc: Document = Jsoup.parse(h)
    assert("<img>\n<svg>\n <image />\n</svg>" == doc.body.html)
  }

  test("handlesInvalidDoctypes") {
    var doc: Document = Jsoup.parse("<!DOCTYPE>")
    assert("<!doctype> <html> <head></head> <body></body> </html>" == Strings.normaliseWhitespace(doc.outerHtml))
    doc = Jsoup.parse("<!DOCTYPE><html><p>Foo</p></html>")
    assert("<!doctype> <html> <head></head> <body> <p>Foo</p> </body> </html>" == Strings.normaliseWhitespace(doc.outerHtml))
    doc = Jsoup.parse("<!DOCTYPE \u0000>")
    assert("<!doctype �> <html> <head></head> <body></body> </html>" == Strings.normaliseWhitespace(doc.outerHtml))
  }

  test("handlesManyChildren") {
    // Arrange
    val longBody: StringBuilder = new StringBuilder(500000)
    for (i <- 0 to 24999) {
      longBody.append(i).append("<br>")
    }
    // Act
    val start: Long = System.currentTimeMillis
    val doc: Document = Parser.parseBodyFragment(longBody.toString(), "")
    // Assert
    assert(50000 == doc.body.childNodeSize)
    val duration = System.currentTimeMillis - start
    //@todo seems that scala performance is really a big issue compared with java, so here need to see if there are good solutions to optimize the code
    assert(duration < 15000)
  }
}
