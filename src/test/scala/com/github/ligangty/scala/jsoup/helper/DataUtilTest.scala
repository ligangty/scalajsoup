package com.github.ligangty.scala.jsoup.helper

import java.nio.ByteBuffer
import java.nio.charset.Charset

import com.github.ligangty.scala.jsoup.nodes.Document
import com.github.ligangty.scala.jsoup.parser.Parser
import org.scalatest.FunSuite

class DataUtilTest extends FunSuite{

  test("testCharset") {
    assert("utf-8"==DataUtil.getCharsetFromContentType("text/html;charset=utf-8 "))
    assert("UTF-8"==DataUtil.getCharsetFromContentType("text/html; charset=UTF-8"))
    assert("ISO-8859-1"==DataUtil.getCharsetFromContentType("text/html; charset=ISO-8859-1"))
    assert(null==DataUtil.getCharsetFromContentType("text/html"))
    assert(null==DataUtil.getCharsetFromContentType(null))
    assert(null==DataUtil.getCharsetFromContentType("text/html;charset=Unknown"))
  }

  test("testQuotedCharset") {
    assert("utf-8"==DataUtil.getCharsetFromContentType("text/html; charset=\"utf-8\""))
    assert("UTF-8"==DataUtil.getCharsetFromContentType("text/html;charset=\"UTF-8\""))
    assert("ISO-8859-1"==DataUtil.getCharsetFromContentType("text/html; charset=\"ISO-8859-1\""))
    assert(null==DataUtil.getCharsetFromContentType("text/html; charset=\"Unsupported\""))
    assert("UTF-8"==DataUtil.getCharsetFromContentType("text/html; charset='UTF-8'"))
  }

  test("discardsSpuriousByteOrderMark") {
    val html: String = "\uFEFF<html><head><title>One</title></head><body>Two</body></html>"
    val buffer: ByteBuffer = Charset.forName("UTF-8").encode(html)
    val doc: Document = DataUtil.parseByteData(buffer, "UTF-8", "http://foo.com/", Parser.htmlParser)
    assert("One"==doc.head.text)
  }

  test("discardsSpuriousByteOrderMarkWhenNoCharsetSet") {
    val html: String = "\uFEFF<html><head><title>One</title></head><body>Two</body></html>"
    val buffer: ByteBuffer = Charset.forName("UTF-8").encode(html)
    val doc: Document = DataUtil.parseByteData(buffer, null, "http://foo.com/", Parser.htmlParser)
    assert("One"==doc.head.text)
    assert("UTF-8"==doc.outputSettings.charset.displayName)
  }

  test("shouldNotThrowExceptionOnEmptyCharset") {
    assert(null==DataUtil.getCharsetFromContentType("text/html; charset="))
    assert(null==DataUtil.getCharsetFromContentType("text/html; charset=;"))
  }

  test("shouldSelectFirstCharsetOnWeirdMultileCharsetsInMetaTags") {
    assert("ISO-8859-1"==DataUtil.getCharsetFromContentType("text/html; charset=ISO-8859-1, charset=1251"))
  }

  test("shouldCorrectCharsetForDuplicateCharsetString") {
    assert("iso-8859-1"==DataUtil.getCharsetFromContentType("text/html; charset=charset=iso-8859-1"))
  }

  test("shouldReturnNullForIllegalCharsetNames") {
    assert(null==DataUtil.getCharsetFromContentType("text/html; charset=$HJKDF§$/("))
  }

  test("generatesMimeBoundaries") {
    val m1: String = DataUtil.mimeBoundary
    val m2: String = DataUtil.mimeBoundary
    assert(DataUtil.boundaryLength==m1.length)
    assert(DataUtil.boundaryLength==m2.length)
    assert(m1 ne m2)
  }
}
