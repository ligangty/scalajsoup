package com.github.ligangty.scala.jsoup.helper

import java.io.File
import com.github.ligangty.scala.jsoup.integration.ParseTest
import org.w3c.dom
import com.github.ligangty.scala.jsoup.{Jsoup, nodes}

import org.scalatest.FunSuite

class W3CDomTest extends FunSuite {

  test("simpleConversion") {
    val html: String = "<html><head><title>W3c</title></head><body><p class='one' id=12>Text</p><!-- comment --><invalid>What<script>alert('!')"
    val doc: nodes.Document = Jsoup.parse(html)
    val w3c: W3CDom = new W3CDom
    val wDoc: dom.Document = w3c.fromJsoup(doc)
    val out: String = w3c.asString(wDoc)
    assert("<html>\n" + "<head>\n" + "<META http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\">\n" + "<title>W3c</title>\n" + "</head>\n" + "<body>\n" + "<p class=\"one\" id=\"12\">Text</p>\n" + "<!-- comment -->\n" + "<invalid>What<script>alert('!')</script>\n" + "</invalid>\n" + "</body>\n" + "</html>\n" == out)
  }

  test("convertsGoogle") {
    val in: File = ParseTest.getFile("/htmltests/google-ipod.html")
    val doc: nodes.Document = Jsoup.parse(in, "UTF8")
    val w3c: W3CDom = new W3CDom
    val wDoc: dom.Document = w3c.fromJsoup(doc)
    val out: String = w3c.asString(wDoc)
    assert(out.contains("ipod"))
  }
}
