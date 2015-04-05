package com.github.ligangty.scala.jsoup.parser

import com.github.ligangty.scala.jsoup.nodes.Document.OutputSettings.Syntax
import com.github.ligangty.scala.jsoup.{Jsoup, TextUtil}
import com.github.ligangty.scala.jsoup.nodes.{Node, Document}
import org.scalatest.FunSuite

class XmlTreeBuilderTest extends FunSuite {

  test("testSimpleXmlParse") {
    val xml: String = "<doc id=2 href='/bar'>Foo <br /><link>One</link><link>Two</link></doc>"
    val tb: XmlTreeBuilder = new XmlTreeBuilder
    val doc: Document = tb.parse(xml, "http://foo.com/")
    assert("<doc id=\"2\" href=\"/bar\">Foo <br /><link>One</link><link>Two</link></doc>" == TextUtil.stripNewlines(doc.html))
    assert(doc.getElementById("2").absUrl("href") == "http://foo.com/bar")
  }

  test("testPopToClose") {
    val xml: String = "<doc><val>One<val>Two</val></bar>Three</doc>"
    val tb: XmlTreeBuilder = new XmlTreeBuilder
    val doc: Document = tb.parse(xml, "http://foo.com/")
    assert("<doc><val>One<val>Two</val>Three</val></doc>" == TextUtil.stripNewlines(doc.html))
  }

  test("testCommentAndDocType") {
    val xml: String = "<!DOCTYPE html><!-- a comment -->One <qux />Two"
    val tb: XmlTreeBuilder = new XmlTreeBuilder
    val doc: Document = tb.parse(xml, "http://foo.com/")
    assert("<!DOCTYPE html><!-- a comment -->One <qux />Two" == TextUtil.stripNewlines(doc.html))
  }

  test("testSupplyParserToJsoupClass") {
//    val xml: String = "<doc><val>One<val>Two</val></bar>Three</doc>"
//    val doc: Document = Jsoup.parse(xml, "http://foo.com/", Parser.xmlParser)
//    assert("<doc><val>One<val>Two</val>Three</val></doc>" == TextUtil.stripNewlines(doc.html))
  }

  ignore("testSupplyParserToConnection") {
//    val xmlUrl: String = "http://direct.infohound.net/tools/jsoup-xml-test.xml"
//    val xmlDoc: Document = Jsoup.connect(xmlUrl).parser(Parser.xmlParser).get
//    val htmlDoc: Document = Jsoup.connect(xmlUrl).get
//    assert("<doc><val>One<val>Two</val>Three</val></doc>" == TextUtil.stripNewlines(xmlDoc.html))
//    assertNotSame(htmlDoc, xmlDoc)
//    assert(1 == htmlDoc.select("head").size)
//    assert(0 == xmlDoc.select("head").size)
  }

  test("testSupplyParserToDataStream") {
//    val xmlFile: File = new File(classOf[XmlTreeBuilder].getResource("/htmltests/xml-test.xml").toURI)
//    val inStream: InputStream = new FileInputStream(xmlFile)
//    val doc: Document = Jsoup.parse(inStream, null, "http://foo.com", Parser.xmlParser)
//    assert("<doc><val>One<val>Two</val>Three</val></doc>" == TextUtil.stripNewlines(doc.html))
  }

  test("testDoesNotForceSelfClosingKnownTags") {
//    val htmlDoc: Document = Jsoup.parse("<br>one</br>")
//    assert("<br>one\n<br>" == htmlDoc.body.html)
//    val xmlDoc: Document = Jsoup.parse("<br>one</br>", "", Parser.xmlParser)
//    assert("<br>one</br>" == xmlDoc.html)
  }

  test("handlesXmlDeclarationAsDeclaration") {
//    val html: String = "<?xml encoding='UTF-8' ?><body>One</body><!-- comment -->"
//    val doc: Document = Jsoup.parse(html, "", Parser.xmlParser)
//    assert("<?xml encoding='UTF-8' ?> <body> One </body> <!-- comment -->" == StringUtil.normaliseWhitespace(doc.outerHtml))
//    assert("#declaration" == doc.childNode(0).nodeName)
//    assert("#comment" == doc.childNode(2).nodeName)
  }

  test("xmlFragment") {
//    val xml: String = "<one src='/foo/' />Two<three><four /></three>"
//    val nodes: List[Node] = Parser.parseXmlFragment(xml, "http://example.com/")
//    assert(3 == nodes.size)
//    assert("http://example.com/foo/" == nodes.get(0).absUrl("src"))
//    assert("one" == nodes.get(0).nodeName)
//    assert("Two" == (nodes.get(1).asInstanceOf[TextNode]).text)
  }

  test("xmlParseDefaultsToHtmlOutputSyntax") {
//    val doc: Document = Jsoup.parse("x", "", Parser.xmlParser)
//    assert(Syntax.xml == doc.outputSettings.syntax)
  }
}
