package com.github.ligangty.scala.jsoup.nodes

import java.io.File
import java.nio.charset.Charset

import com.github.ligangty.scala.jsoup.{TextUtil, Jsoup}
import com.github.ligangty.scala.jsoup.integration.ParseTest
import com.github.ligangty.scala.jsoup.nodes.Document.OutputSettings
import com.github.ligangty.scala.jsoup.nodes.Entities.{EXTENDED, BASE}
import org.scalatest.FunSuite

/**
 * test for Document
 */
class DocumentTest extends FunSuite {

  test("setTextPreservesDocumentStructure") {
    val doc: Document = Jsoup.parse("<p>Hello</p>")
    doc.text("Replaced")
    assert("Replaced" == doc.text())
    assert("Replaced" == doc.body.text())
    assert(1 == doc.select("head").size)
  }

  test("testTitles") {
    val noTitle: Document = Jsoup.parse("<p>Hello</p>")
    val withTitle: Document = Jsoup.parse("<title>First</title><title>Ignore</title><p>Hello</p>")
    assert("" == noTitle.title)
    noTitle.title("Hello")
    assert("Hello" == noTitle.title)
    assert("Hello" == noTitle.select("title").first().text)
    assert("First" == withTitle.title)
    withTitle.title("Hello")
    assert("Hello" == withTitle.title)
    assert("Hello" == withTitle.select("title").first().text)
    val normaliseTitle: Document = Jsoup.parse("<title>   Hello\nthere   \n   now   \n")
    assert("Hello there now" == normaliseTitle.title)
  }

  test("testOutputEncoding") {
    val doc: Document = Jsoup.parse("<p title=π>π & < > </p>")
    assert("<p title=\"π\">π &amp; &lt; &gt; </p>" == doc.body.html)
    assert("UTF-8" == doc.outputSettings.charset.displayName)
    doc.outputSettings.charset("ascii")
    assert(Entities.BASE == doc.outputSettings.escapeMode)
    assert("<p title=\"&#x3c0;\">&#x3c0; &amp; &lt; &gt; </p>" == doc.body.html)
    doc.outputSettings.escapeMode(Entities.EXTENDED)
    assert("<p title=\"&pi;\">&pi; &amp; &lt; &gt; </p>" == doc.body.html)
  }

  test("testXhtmlReferences") {
    val doc: Document = Jsoup.parse("&lt; &gt; &amp; &quot; &apos; &times;")
    doc.outputSettings.escapeMode(Entities.XHTML)
    assert("&lt; &gt; &amp; \" ' ×" == doc.body.html)
  }

  test("testNormalisesStructure") {
    val doc: Document = Jsoup.parse("<html><head><script>one</script><noscript><p>two</p></noscript></head><body><p>three</p></body><p>four</p></html>")
    assert("<html><head><script>one</script><noscript>&lt;p&gt;two</noscript></head><body><p>three</p><p>four</p></body></html>" == TextUtil.stripNewlines(doc.html))
  }

  test("testClone") {
    val doc: Document = Jsoup.parse("<title>Hello</title> <p>One<p>Two")
    val clone: Document = doc.clone()
    assert("<html><head><title>Hello</title> </head><body><p>One</p><p>Two</p></body></html>" == TextUtil.stripNewlines(clone.html))
    clone.title("Hello there")
    clone.select("p").first().text("One more").attr("id", "1")
    assert("<html><head><title>Hello there</title> </head><body><p id=\"1\">One more</p><p>Two</p></body></html>" == TextUtil.stripNewlines(clone.html))
    assert("<html><head><title>Hello</title> </head><body><p>One</p><p>Two</p></body></html>" == TextUtil.stripNewlines(doc.html))
  }

  test("testClonesDeclarations") {
    val doc: Document = Jsoup.parse("<!DOCTYPE html><html><head><title>Doctype test")
    val clone: Document = doc.clone()
    assert(doc.html == clone.html)
    assert("<!doctype html><html><head><title>Doctype test</title></head><body></body></html>" == TextUtil.stripNewlines(clone.html))
  }

  test("testLocation") {
    var in: File = ParseTest.getFile("/htmltests/yahoo-jp.html")
    var doc: Document = Jsoup.parse(in, "UTF-8", "http://www.yahoo.co.jp/index.html")
    var location: String = doc.location
    var baseUri: String = doc.baseUri
    assert("http://www.yahoo.co.jp/index.html" == location)
    assert("http://www.yahoo.co.jp/_ylh=X3oDMTB0NWxnaGxsBF9TAzIwNzcyOTYyNjUEdGlkAzEyBHRtcGwDZ2Ex/" == baseUri)
    in = ParseTest.getFile("/htmltests/nyt-article-1.html")
    doc = Jsoup.parse(in, null, "http://www.nytimes.com/2010/07/26/business/global/26bp.html?hp")
    location = doc.location
    baseUri = doc.baseUri
    assert("http://www.nytimes.com/2010/07/26/business/global/26bp.html?hp" == location)
    assert("http://www.nytimes.com/2010/07/26/business/global/26bp.html?hp" == baseUri)
  }

  test("testHtmlAndXmlSyntax") {
    val h: String = "<!DOCTYPE html><body><img async checked='checked' src='&<>\"'>&lt;&gt;&amp;&quot;<foo />bar"
    val doc: Document = Jsoup.parse(h)
    doc.outputSettings.syntax(Document.OutputSettings.Syntax.html)
    assert("<!doctype html>\n" + "<html>\n" + " <head></head>\n" + " <body>\n" + "  <img async checked src=\"&amp;<>&quot;\">&lt;&gt;&amp;\"\n" + "  <foo />bar\n" + " </body>\n" + "</html>" == doc.html)
    doc.outputSettings.syntax(Document.OutputSettings.Syntax.xml)
    assert("<!DOCTYPE html>\n" + "<html>\n" + " <head></head>\n" + " <body>\n" + "  <img async=\"\" checked=\"checked\" src=\"&amp;<>&quot;\" />&lt;&gt;&amp;\"\n" + "  <foo />bar\n" + " </body>\n" + "</html>" == doc.html)
  }

  test("htmlParseDefaultsToHtmlOutputSyntax") {
    val doc: Document = Jsoup.parse("x")
    assert(Document.OutputSettings.Syntax.html == doc.outputSettings.syntax)
  }

  ignore("testOverflowClone") {
    val builder: StringBuilder = new StringBuilder
    for (i <- 0 to 99999) {
      builder.insert(0, "<i>")
      builder.append("</i>")
    }
    val doc: Document = Jsoup.parse(builder.toString())
    doc.clone()
  }

  test("Document.OutputSettings") {
    val docSetting = new OutputSettings
    assert(Charset.forName("UTF-8") == docSetting.charset)
    assert(BASE == docSetting.escapeMode)
    assert(docSetting.prettyPrint)
    assert(!docSetting.outline)
    assert(docSetting.indentAmount == 1)

    docSetting.charset(Charset.forName("ISO-8859-1")).outline(true).escapeMode(EXTENDED).indentAmount(4).prettyPrint(false)

    assert(Charset.forName("ISO-8859-1") == docSetting.charset)
    assert(EXTENDED == docSetting.escapeMode)
    assert(!docSetting.prettyPrint)
    assert(docSetting.outline)
    assert(docSetting.indentAmount == 4)

  }

}
