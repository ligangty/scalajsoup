package com.github.ligangty.scala.jsoup.nodes

import com.github.ligangty.scala.jsoup.Jsoup
import com.github.ligangty.scala.jsoup.nodes.Entities._
import org.scalatest.FunSuite

/**
 * Test for Entities
 */
class EntitiesTest extends FunSuite {

  test("escape") {
    val text: String = "Hello &<> Å å π 新 there ¾ © »"
    val escapedAscii: String = Entities.escape(text, new Document.OutputSettings().charset("ascii").escapeMode(BASE))
    val escapedAsciiFull: String = Entities.escape(text, new Document.OutputSettings().charset("ascii").escapeMode(EXTENDED))
    val escapedAsciiXhtml: String = Entities.escape(text, new Document.OutputSettings().charset("ascii").escapeMode(XHTML))
    val escapedUtfFull: String = Entities.escape(text, new Document.OutputSettings().charset("UTF-8").escapeMode(BASE))
    val escapedUtfMin: String = Entities.escape(text, new Document.OutputSettings().charset("UTF-8").escapeMode(XHTML))
    assert("Hello &amp;&lt;&gt; &Aring; &aring; &#x3c0; &#x65b0; there &frac34; &copy; &raquo;" == escapedAscii)
    assert("Hello &amp;&lt;&gt; &angst; &aring; &pi; &#x65b0; there &frac34; &copy; &raquo;" == escapedAsciiFull)
    assert("Hello &amp;&lt;&gt; &#xc5; &#xe5; &#x3c0; &#x65b0; there &#xbe; &#xa9; &#xbb;" == escapedAsciiXhtml)
    assert("Hello &amp;&lt;&gt; Å å π 新 there ¾ © »" == escapedUtfFull)
    assert("Hello &amp;&lt;&gt; Å å π 新 there ¾ © »" == escapedUtfMin)
    // odd that it's defined as aring in base but angst in full

    // round trip
    assert(text == Entities.unescape(escapedAscii))
    assert(text == Entities.unescape(escapedAsciiFull))
    assert(text == Entities.unescape(escapedAsciiXhtml))
    assert(text == Entities.unescape(escapedUtfFull))
    assert(text == Entities.unescape(escapedUtfMin))
  }

  test("escapeSupplementaryCharacter") {
    val text: String = new String(Character.toChars(135361))
    val escapedAscii: String = Entities.escape(text, new Document.OutputSettings().charset("ascii").escapeMode(Entities.BASE))
    assert("&#x210c1;" == escapedAscii)
    val escapedUtf: String = Entities.escape(text, new Document.OutputSettings().charset("UTF-8").escapeMode(Entities.BASE))
    assert(text == escapedUtf)
  }

  test("unescape") {
    val text: String = "Hello &amp;&LT&gt; &reg &angst; &angst &#960; &#960 &#x65B0; there &! &frac34; &copy; &COPY;"
    assert("Hello &<> ® Å &angst π π 新 there &! ¾ © ©" == Entities.unescape(text))

    assert("&0987654321; &unknown" == Entities.unescape("&0987654321; &unknown"))
  }

  test("strictUnescape") {
    val text: String = "Hello &amp= &amp;"
    assert("Hello &amp= &" == Entities.unescape(text, true))
    assert("Hello &= &" == Entities.unescape(text))
    assert("Hello &= &" == Entities.unescape(text, false))
  }

  test("caseSensitive") {
    val unescaped: String = "Ü ü & &"
    assert("&Uuml; &uuml; &amp; &amp;" == Entities.escape(unescaped, new Document.OutputSettings().charset("ascii").escapeMode(Entities.EXTENDED)))
    val escaped: String = "&Uuml; &uuml; &amp; &AMP"
    assert("Ü ü & &" == Entities.unescape(escaped))
  }

  test("quoteReplacements") {
    val escaped: String = "&#92; &#36;"
    val unescaped: String = "\\ $"
    assert(unescaped == Entities.unescape(escaped))
  }

  test("letterDigitEntities") {
    val html: String = "<p>&sup1;&sup2;&sup3;&frac14;&frac12;&frac34;</p>"
    val doc: Document = Jsoup.parse(html)
    doc.outputSettings.charset("ascii")
    val p: Element = doc.select("p").first()
    assert("&sup1;&sup2;&sup3;&frac14;&frac12;&frac34;" == p.html)
    assert("¹²³¼½¾" == p.text)
    doc.outputSettings.charset("UTF-8")
    assert("¹²³¼½¾" == p.html)
  }

  test("noSpuriousDecodes") {
    val string: String = "http://www.foo.com?a=1&num_rooms=1&children=0&int=VA&b=2"
    assert(string == Entities.unescape(string))
  }

  test("isBaseNamedEntity") {
    assert(isBaseNamedEntity("AElig"))
    assert(isBaseNamedEntity("yuml"))
    assert(!isBaseNamedEntity("ABC"))
  }

  test("isNamedEntity") {
    assert(isNamedEntity("AElig"))
    assert(isNamedEntity("zwnj"))
    assert(!isNamedEntity("ABC"))
  }

  test("getCharacterByName") {
    assert(Integer.parseInt("000C6", 16).toChar == getCharacterByName("AElig"))
  }

  test("Entities.EscapeMode Enumeration") {
    assert(XHTML(0x00022.toChar) == "quot")
    intercept[NoSuchElementException] {
      XHTML(0x000C6.toChar)
    }
    assert(BASE(0x000C6.toChar) == "AElig")
    intercept[NoSuchElementException] {
      BASE(0x0200C.toChar)
    }
    assert(EXTENDED(0x0200C.toChar) == "zwnj")
    intercept[NoSuchElementException] {
      EXTENDED(0x0240F.toChar)
    }
  }

}
