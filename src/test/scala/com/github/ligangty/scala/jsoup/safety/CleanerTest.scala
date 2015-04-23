package com.github.ligangty.scala.jsoup.safety

import com.github.ligangty.scala.jsoup.nodes.{Entities, Document}
import com.github.ligangty.scala.jsoup.{TextUtil, Jsoup}
import org.scalatest.FunSuite

/**
 * Tests for the cleaner.
 */
class CleanerTest extends FunSuite {

  test("simpleBehaviourTest") {
    val h: String = "<div><p class=foo><a href='http://evil.com'>Hello <b id=bar>there</b>!</a></div>"
    val cleanHtml: String = Jsoup.clean(h, Whitelist.simpleText)
    assert("Hello <b>there</b>!" == TextUtil.stripNewlines(cleanHtml))
  }

  test("simpleBehaviourTest2") {
    val h: String = "Hello <b>there</b>!"
    val cleanHtml: String = Jsoup.clean(h, Whitelist.simpleText)
    assert("Hello <b>there</b>!" == TextUtil.stripNewlines(cleanHtml))
  }

  test("basicBehaviourTest") {
    val h: String = "<div><p><a href='javascript:sendAllMoney()'>Dodgy</a> <A HREF='HTTP://nice.com'>Nice</a></p><blockquote>Hello</blockquote>"
    val cleanHtml: String = Jsoup.clean(h, Whitelist.basic)
    assert("<p><a rel=\"nofollow\">Dodgy</a> <a href=\"http://nice.com\" rel=\"nofollow\">Nice</a></p><blockquote>Hello</blockquote>" == TextUtil.stripNewlines(cleanHtml))
  }

  test("basicWithImagesTest") {
    val h: String = "<div><p><img src='http://example.com/' alt=Image></p><p><img src='ftp://ftp.example.com'></p></div>"
    val cleanHtml: String = Jsoup.clean(h, Whitelist.basicWithImages)
    assert("<p><img src=\"http://example.com/\" alt=\"Image\"></p><p><img></p>" == TextUtil.stripNewlines(cleanHtml))
  }

  test("testRelaxed") {
    val h: String = "<h1>Head</h1><table><tr><td>One<td>Two</td></tr></table>"
    val cleanHtml: String = Jsoup.clean(h, Whitelist.relaxed)
    assert("<h1>Head</h1><table><tbody><tr><td>One</td><td>Two</td></tr></tbody></table>" == TextUtil.stripNewlines(cleanHtml))
  }

  test("testRemoveTags") {
    val h: String = "<div><p><A HREF='HTTP://nice.com'>Nice</a></p><blockquote>Hello</blockquote>"
    val cleanHtml: String = Jsoup.clean(h, Whitelist.basic.removeTags("a"))
    assert("<p>Nice</p><blockquote>Hello</blockquote>" == TextUtil.stripNewlines(cleanHtml))
  }

  test("testRemoveAttributes") {
    val h: String = "<div><p>Nice</p><blockquote cite='http://example.com/quotations'>Hello</blockquote>"
    val cleanHtml: String = Jsoup.clean(h, Whitelist.basic.removeAttributes("blockquote", "cite"))
    assert("<p>Nice</p><blockquote>Hello</blockquote>" == TextUtil.stripNewlines(cleanHtml))
  }

  test("testRemoveEnforcedAttributes") {
    val h: String = "<div><p><A HREF='HTTP://nice.com'>Nice</a></p><blockquote>Hello</blockquote>"
    val cleanHtml: String = Jsoup.clean(h, Whitelist.basic.removeEnforcedAttribute("a", "rel"))
    assert("<p><a href=\"http://nice.com\">Nice</a></p><blockquote>Hello</blockquote>" == TextUtil.stripNewlines(cleanHtml))
  }

  test("testRemoveProtocols") {
    val h: String = "<p>Contact me <a href='mailto:info@example.com'>here</a></p>"
    val cleanHtml: String = Jsoup.clean(h, Whitelist.basic.removeProtocols("a", "href", "ftp", "mailto"))
    assert("<p>Contact me <a rel=\"nofollow\">here</a></p>" == TextUtil.stripNewlines(cleanHtml))
  }

  test("testDropComments") {
    val h: String = "<p>Hello<!-- no --></p>"
    val cleanHtml: String = Jsoup.clean(h, Whitelist.relaxed)
    assert("<p>Hello</p>" == cleanHtml)
  }

  test("testDropXmlProc") {
    val h: String = "<?import namespace=\"xss\"><p>Hello</p>"
    val cleanHtml: String = Jsoup.clean(h, Whitelist.relaxed)
    assert("<p>Hello</p>" == cleanHtml)
  }

  test("testDropScript") {
    val h: String = "<SCRIPT SRC=//ha.ckers.org/.j><SCRIPT>alert(/XSS/.source)</SCRIPT>"
    val cleanHtml: String = Jsoup.clean(h, Whitelist.relaxed)
    assert("" == cleanHtml)
  }

  test("testDropImageScript") {
    val h: String = "<IMG SRC=\"javascript:alert('XSS')\">"
    val cleanHtml: String = Jsoup.clean(h, Whitelist.relaxed)
    assert("<img>" == cleanHtml)
  }

  test("testCleanJavascriptHref") {
    val h: String = "<A HREF=\"javascript:document.location='http://www.google.com/'\">XSS</A>"
    val cleanHtml: String = Jsoup.clean(h, Whitelist.relaxed)
    assert("<a>XSS</a>" == cleanHtml)
  }

  test("testCleanAnchorProtocol") {
    val validAnchor: String = "<a href=\"#valid\">Valid anchor</a>"
    val invalidAnchor: String = "<a href=\"#anchor with spaces\">Invalid anchor</a>"
    var cleanHtml: String = Jsoup.clean(validAnchor, Whitelist.relaxed)
    assert("<a>Valid anchor</a>" == cleanHtml)
    cleanHtml = Jsoup.clean(invalidAnchor, Whitelist.relaxed)
    assert("<a>Invalid anchor</a>" == cleanHtml)
    val relaxedWithAnchor: Whitelist = Whitelist.relaxed.addProtocols("a", "href", "#")
    cleanHtml = Jsoup.clean(validAnchor, relaxedWithAnchor)
    assert(validAnchor == cleanHtml)
    cleanHtml = Jsoup.clean(invalidAnchor, relaxedWithAnchor)
    assert("<a>Invalid anchor</a>" == cleanHtml)
  }

  test("testDropsUnknownTags") {
    val h: String = "<p><custom foo=true>Test</custom></p>"
    val cleanHtml: String = Jsoup.clean(h, Whitelist.relaxed)
    assert("<p>Test</p>" == cleanHtml)
  }

  test("testHandlesEmptyAttributes") {
    val h: String = "<img alt=\"\" src= unknown=''>"
    val cleanHtml: String = Jsoup.clean(h, Whitelist.basicWithImages)
    assert("<img alt=\"\">" == cleanHtml)
  }

  test("testIsValid") {
    val ok: String = "<p>Test <b><a href='http://example.com/'>OK</a></b></p>"
    val nok1: String = "<p><script></script>Not <b>OK</b></p>"
    val nok2: String = "<p align=right>Test Not <b>OK</b></p>"
    val nok3: String = "<!-- comment --><p>Not OK</p>"
    assert(Jsoup.isValid(ok, Whitelist.basic))
    assert(!Jsoup.isValid(nok1, Whitelist.basic))
    assert(!Jsoup.isValid(nok2, Whitelist.basic))
    assert(!Jsoup.isValid(nok3, Whitelist.basic))
  }

  test("resolvesRelativeLinks") {
    val html: String = "<a href='/foo'>Link</a><img src='/bar'>"
    val clean: String = Jsoup.clean(html, "http://example.com/", Whitelist.basicWithImages)
    assert("<a href=\"http://example.com/foo\" rel=\"nofollow\">Link</a>\n<img src=\"http://example.com/bar\">" == clean)
  }

  test("preservesRelativeLinksIfConfigured") {
    val html: String = "<a href='/foo'>Link</a><img src='/bar'> <img src='javascript:alert()'>"
    val clean: String = Jsoup.clean(html, "http://example.com/", Whitelist.basicWithImages.preserveRelativeLinks(true))
    assert("<a href=\"/foo\" rel=\"nofollow\">Link</a>\n<img src=\"/bar\"> \n<img>" == clean)
  }

  test("dropsUnresolvableRelativeLinks") {
    val html: String = "<a href='/foo'>Link</a>"
    val clean: String = Jsoup.clean(html, Whitelist.basic)
    assert("<a rel=\"nofollow\">Link</a>" == clean)
  }

  test("handlesCustomProtocols") {
    val html: String = "<img src='cid:12345' /> <img src='data:gzzt' />"
    val dropped: String = Jsoup.clean(html, Whitelist.basicWithImages)
    assert("<img> \n<img>" == dropped)
    val preserved: String = Jsoup.clean(html, Whitelist.basicWithImages.addProtocols("img", "src", "cid", "data"))
    assert("<img src=\"cid:12345\"> \n<img src=\"data:gzzt\">" == preserved)
  }

  test("handlesAllPseudoTag") {
    val html: String = "<p class='foo' src='bar'><a class='qux'>link</a></p>"
    val whitelist: Whitelist = new Whitelist().addAttributes(":all", "class").addAttributes("p", "style").addTags("p", "a")
    val clean: String = Jsoup.clean(html, whitelist)
    assert("<p class=\"foo\"><a class=\"qux\">link</a></p>" == clean)
  }

  test("addsTagOnAttributesIfNotSet") {
    val html: String = "<p class='foo' src='bar'>One</p>"
    val whitelist: Whitelist = new Whitelist().addAttributes("p", "class")
    val clean: String = Jsoup.clean(html, whitelist)
    assert("<p class=\"foo\">One</p>" == clean)
  }

  test("supplyOutputSettings") {
    val os: Document.OutputSettings = new Document.OutputSettings
    os.prettyPrint(false)
    os.escapeMode(Entities.EXTENDED)
    os.charset("ascii")
    val html: String = "<div><p>&bernou;</p></div>"
    val customOut: String = Jsoup.clean(html, "http://foo.com/", Whitelist.relaxed, os)
    val defaultOut: String = Jsoup.clean(html, "http://foo.com/", Whitelist.relaxed)
    assert(defaultOut ne customOut)
    assert("<div><p>&bernou;</p></div>" == customOut)
    assert("<div>\n" + " <p>ℬ</p>\n" + "</div>" == defaultOut)
    os.charset("ASCII")
    os.escapeMode(Entities.BASE)
    val customOut2: String = Jsoup.clean(html, "http://foo.com/", Whitelist.relaxed, os)
    assert("<div><p>&#x212c;</p></div>" == customOut2)
  }

  test("handlesFramesets") {
    val dirty: String = "<html><head><script></script><noscript></noscript></head><frameset><frame src=\"foo\" /><frame src=\"foo\" /></frameset></html>"
    val clean: String = Jsoup.clean(dirty, Whitelist.basic)
    assert("" == clean)
    val dirtyDoc: Document = Jsoup.parse(dirty)
    val cleanDoc: Document = new Cleaner(Whitelist.basic).clean(dirtyDoc)
    assert(cleanDoc != null)
    assert(0 == cleanDoc.body.childNodeSize)
  }

  test("cleansInternationalText") {
    assert("привет" == Jsoup.clean("привет", Whitelist.none))
  }

  test("testScriptTagInWhiteList") {
    val whitelist: Whitelist = Whitelist.relaxed
    whitelist.addTags("script")
    assert(Jsoup.isValid("Hello<script>alert('Doh')</script>World !", whitelist))
  }
}
