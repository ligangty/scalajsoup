package com.github.ligangty.scala.jsoup.nodes

import java.lang._
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
    //    assert(text==Entities.unescape(escapedAscii))
    //    assert(text== Entities.unescape(escapedAsciiFull))
    //    assert(text==Entities.unescape(escapedAsciiXhtml))
    //    assert(text==Entities.unescape(escapedUtfFull))
    //    assert(text==Entities.unescape(escapedUtfMin))
  }

  ignore("unescape"){
    fail("not implemented yet!")
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
