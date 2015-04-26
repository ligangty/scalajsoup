package com.github.ligangty.scala.jsoup.select

import com.github.ligangty.scala.jsoup.Jsoup
import com.github.ligangty.scala.jsoup.nodes.{Element, Document}
import org.scalatest.FunSuite

/**
 * Tests that the selector selects correctly.
 *
 */
class SelectorTest extends FunSuite {

  test("testByTag") {
    val els: Elements = Jsoup.parse("<div id=1><div id=2><p>Hello</p></div></div><div id=3>").select("div")
    assert(3 == els.size)
    assert("1" == els.get(0).id)
    assert("2" == els.get(1).id)
    assert("3" == els.get(2).id)
    val none: Elements = Jsoup.parse("<div id=1><div id=2><p>Hello</p></div></div><div id=3>").select("span")
    assert(0 == none.size)
  }

  test("testById") {
    val els: Elements = Jsoup.parse("<div><p id=foo>Hello</p><p id=foo>Foo two!</p></div>").select("#foo")
    assert(2 == els.size)
    assert("Hello" == els.get(0).text)
    assert("Foo two!" == els.get(1).text)
    val none: Elements = Jsoup.parse("<div id=1></div>").select("#foo")
    assert(0 == none.size)
  }

  test("testByClass") {
    val els: Elements = Jsoup.parse("<p id=0 class='one two'><p id=1 class='one'><p id=2 class='two'>").select("p.one")
    assert(2 == els.size)
    assert("0" == els.get(0).id)
    assert("1" == els.get(1).id)
    val none: Elements = Jsoup.parse("<div class='one'></div>").select(".foo")
    assert(0 == none.size)
    val els2: Elements = Jsoup.parse("<div class='One-Two'></div>").select(".one-two")
    assert(1 == els2.size)
  }

  test("testByAttribute") {
    val h: String = "<div Title=Foo /><div Title=Bar /><div Style=Qux /><div title=Bam /><div title=SLAM />" + "<div data-name='with spaces'/>"
    val doc: Document = Jsoup.parse(h)
    val withTitle: Elements = doc.select("[title]")
    assert(4 == withTitle.size)
    val foo: Elements = doc.select("[title=foo]")
    assert(1 == foo.size)
    val foo2: Elements = doc.select("[title=\"foo\"]")
    assert(1 == foo2.size)
    val foo3: Elements = doc.select("[title=\"Foo\"]")
    assert(1 == foo3.size)
    val dataName: Elements = doc.select("[data-name=\"with spaces\"]")
    assert(1 == dataName.size)
    assert("with spaces" == dataName.first().attr("data-name"))
    val not: Elements = doc.select("div[title!=bar]")
    assert(5 == not.size)
    assert("Foo" == not.first().attr("title"))
    val starts: Elements = doc.select("[title^=ba]")
    assert(2 == starts.size)
    assert("Bar" == starts.first().attr("title"))
    assert("Bam" == starts.last.attr("title"))
    val ends: Elements = doc.select("[title$=am]")
    assert(2 == ends.size)
    assert("Bam" == ends.first().attr("title"))
    assert("SLAM" == ends.last.attr("title"))
    val contains: Elements = doc.select("[title*=a]")
    assert(3 == contains.size)
    assert("Bar" == contains.first().attr("title"))
    assert("SLAM" == contains.last.attr("title"))
  }

  test("testNamespacedTag") {
    val doc: Document = Jsoup.parse("<div><abc:def id=1>Hello</abc:def></div> <abc:def class=bold id=2>There</abc:def>")
    val byTag: Elements = doc.select("abc|def")
    assert(2 == byTag.size)
    assert("1" == byTag.first().id)
    assert("2" == byTag.last.id)
    val byAttr: Elements = doc.select(".bold")
    assert(1 == byAttr.size)
    assert("2" == byAttr.last.id)
    val byTagAttr: Elements = doc.select("abc|def.bold")
    assert(1 == byTagAttr.size)
    assert("2" == byTagAttr.last.id)
    val byContains: Elements = doc.select("abc|def:contains(e)")
    assert(2 == byContains.size)
    assert("1" == byContains.first().id)
    assert("2" == byContains.last.id)
  }

  test("testByAttributeStarting") {
    val doc: Document = Jsoup.parse("<div id=1 data-name=jsoup>Hello</div><p data-val=5 id=2>There</p><p id=3>No</p>")
    var withData: Elements = doc.select("[^data-]")
    assert(2 == withData.size)
    assert("1" == withData.first().id)
    assert("2" == withData.last.id)
    withData = doc.select("p[^data-]")
    assert(1 == withData.size)
    assert("2" == withData.first().id)
  }

  test("testByAttributeRegex") {
    val doc: Document = Jsoup.parse("<p><img src=foo.png id=1><img src=bar.jpg id=2><img src=qux.JPEG id=3><img src=old.gif><img></p>")
    val imgs: Elements = doc.select("img[src~=(?i)\\.(png|jpe?g)]")
    assert(3 == imgs.size)
    assert("1" == imgs.get(0).id)
    assert("2" == imgs.get(1).id)
    assert("3" == imgs.get(2).id)
  }

  test("testByAttributeRegexCharacterClass") {
    val doc: Document = Jsoup.parse("<p><img src=foo.png id=1><img src=bar.jpg id=2><img src=qux.JPEG id=3><img src=old.gif id=4></p>")
    val imgs: Elements = doc.select("img[src~=[o]]")
    assert(2 == imgs.size)
    assert("1" == imgs.get(0).id)
    assert("4" == imgs.get(1).id)
  }

  test("testByAttributeRegexCombined") {
    val doc: Document = Jsoup.parse("<div><table class=x><td>Hello</td></table></div>")
    val els: Elements = doc.select("div table[class~=x|y]")
    assert(1 == els.size)
    assert("Hello" == els.text)
  }

  test("testCombinedWithContains") {
    val doc: Document = Jsoup.parse("<p id=1>One</p><p>Two +</p><p>Three +</p>")
    val els: Elements = doc.select("p#1 + :contains(+)")
    assert(1 == els.size)
    assert("Two +" == els.text)
    assert("p" == els.first().tagName)
  }

  test("testAllElements") {
    val h: String = "<div><p>Hello</p><p><b>there</b></p></div>"
    val doc: Document = Jsoup.parse(h)
    val allDoc: Elements = doc.select("*")
    val allUnderDiv: Elements = doc.select("div *")
    assert(8 == allDoc.size)
    assert(3 == allUnderDiv.size)
    assert("p" == allUnderDiv.first().tagName)
  }

  test("testAllWithClass") {
    val h: String = "<p class=first>One<p class=first>Two<p>Three"
    val doc: Document = Jsoup.parse(h)
    val ps: Elements = doc.select("*.first")
    assert(2 == ps.size)
  }

  test("testGroupOr") {
    val h: String = "<div title=foo /><div title=bar /><div /><p></p><img /><span title=qux>"
    val doc: Document = Jsoup.parse(h)
    val els: Elements = doc.select("p,div,[title]")
    assert(5 == els.size)
    assert("div" == els.get(0).tagName)
    assert("foo" == els.get(0).attr("title"))
    assert("div" == els.get(1).tagName)
    assert("bar" == els.get(1).attr("title"))
    assert("div" == els.get(2).tagName)
    assert(els.get(2).attr("title").length == 0)
    assert(!els.get(2).hasAttr("title"))
    assert("p" == els.get(3).tagName)
    assert("span" == els.get(4).tagName)
  }

  test("testGroupOrAttribute") {
    val h: String = "<div id=1 /><div id=2 /><div title=foo /><div title=bar />"
    val els: Elements = Jsoup.parse(h).select("[id],[title=foo]")
    assert(3 == els.size)
    assert("1" == els.get(0).id)
    assert("2" == els.get(1).id)
    assert("foo" == els.get(2).attr("title"))
  }

  test("descendant") {
    val h: String = "<div class=head><p class=first>Hello</p><p>There</p></div><p>None</p>"
    val doc: Document = Jsoup.parse(h)
    val els: Elements = doc.select(".head p")
    assert(2 == els.size)
    assert("Hello" == els.get(0).text)
    assert("There" == els.get(1).text)
    val p: Elements = doc.select("p.first")
    assert(1 == p.size)
    assert("Hello" == p.get(0).text)
    val empty: Elements = doc.select("p .first")
    assert(0 == empty.size)
  }

  test("and") {
    val h: String = "<div id=1 class='foo bar' title=bar name=qux><p class=foo title=bar>Hello</p></div"
    val doc: Document = Jsoup.parse(h)
    val div: Elements = doc.select("div.foo")
    assert(1 == div.size)
    assert("div" == div.first().tagName)
    val p: Elements = doc.select("div .foo")
    assert(1 == p.size)
    assert("p" == p.first().tagName)
    val div2: Elements = doc.select("div#1.foo.bar[title=bar][name=qux]")
    assert(1 == div2.size)
    assert("div" == div2.first().tagName)
    val p2: Elements = doc.select("div *.foo")
    assert(1 == p2.size)
    assert("p" == p2.first().tagName)
  }

  test("deeperDescendant") {
    val h: String = "<div class=head><p><span class=first>Hello</div><div class=head><p class=first><span>Another</span><p>Again</div>"
    val els: Elements = Jsoup.parse(h).select("div p .first")
    assert(1 == els.size)
    assert("Hello" == els.first().text)
    assert("span" == els.first().tagName)
  }

  test("parentChildElement") {
    val h: String = "<div id=1><div id=2><div id = 3></div></div></div><div id=4></div>"
    val doc: Document = Jsoup.parse(h)
    val divs: Elements = doc.select("div > div")
    assert(2 == divs.size)
    assert("2" == divs.get(0).id)
    assert("3" == divs.get(1).id)
    val div2: Elements = doc.select("div#1 > div")
    assert(1 == div2.size)
    assert("2" == div2.get(0).id)
  }

  test("parentWithClassChild") {
    val h: String = "<h1 class=foo><a href=1 /></h1><h1 class=foo><a href=2 class=bar /></h1><h1><a href=3 /></h1>"
    val doc: Document = Jsoup.parse(h)
    val allAs: Elements = doc.select("h1 > a")
    assert(3 == allAs.size)
    assert("a" == allAs.first().tagName)
    val fooAs: Elements = doc.select("h1.foo > a")
    assert(2 == fooAs.size)
    assert("a" == fooAs.first().tagName)
    val barAs: Elements = doc.select("h1.foo > a.bar")
    assert(1 == barAs.size)
  }

  test("parentChildStar") {
    val h: String = "<div id=1><p>Hello<p><b>there</b></p></div><div id=2><span>Hi</span></div>"
    val doc: Document = Jsoup.parse(h)
    val divChilds: Elements = doc.select("div > *")
    assert(3 == divChilds.size)
    assert("p" == divChilds.get(0).tagName)
    assert("p" == divChilds.get(1).tagName)
    assert("span" == divChilds.get(2).tagName)
  }

  test("multiChildDescent") {
    val h: String = "<div id=foo><h1 class=bar><a href=http://example.com/>One</a></h1></div>"
    val doc: Document = Jsoup.parse(h)
    val els: Elements = doc.select("div#foo > h1.bar > a[href*=example]")
    assert(1 == els.size)
    assert("a" == els.first().tagName)
  }

  test("caseInsensitive") {
    val h: String = "<dIv tItle=bAr><div>"
    val doc: Document = Jsoup.parse(h)
    assert(2 == doc.select("DIV").size)
    assert(1 == doc.select("DIV[TITLE]").size)
    assert(1 == doc.select("DIV[TITLE=BAR]").size)
    assert(0 == doc.select("DIV[TITLE=BARBARELLA").size)
  }

  test("adjacentSiblings") {
    val h: String = "<ol><li>One<li>Two<li>Three</ol>"
    val doc: Document = Jsoup.parse(h)
    val sibs: Elements = doc.select("li + li")
    assert(2 == sibs.size)
    assert("Two" == sibs.get(0).text)
    assert("Three" == sibs.get(1).text)
  }

  test("adjacentSiblingsWithId") {
    val h: String = "<ol><li id=1>One<li id=2>Two<li id=3>Three</ol>"
    val doc: Document = Jsoup.parse(h)
    val sibs: Elements = doc.select("li#1 + li#2")
    assert(1 == sibs.size)
    assert("Two" == sibs.get(0).text)
  }

  test("notAdjacent") {
    val h: String = "<ol><li id=1>One<li id=2>Two<li id=3>Three</ol>"
    val doc: Document = Jsoup.parse(h)
    val sibs: Elements = doc.select("li#1 + li#3")
    assert(0 == sibs.size)
  }

  test("mixCombinator") {
    val h: String = "<div class=foo><ol><li>One<li>Two<li>Three</ol></div>"
    val doc: Document = Jsoup.parse(h)
    val sibs: Elements = doc.select("body > div.foo li + li")
    assert(2 == sibs.size)
    assert("Two" == sibs.get(0).text)
    assert("Three" == sibs.get(1).text)
  }

  test("mixCombinatorGroup") {
    val h: String = "<div class=foo><ol><li>One<li>Two<li>Three</ol></div>"
    val doc: Document = Jsoup.parse(h)
    val els: Elements = doc.select(".foo > ol, ol > li + li")
    assert(3 == els.size)
    assert("ol" == els.get(0).tagName)
    assert("Two" == els.get(1).text)
    assert("Three" == els.get(2).text)
  }

  test("generalSiblings") {
    val h: String = "<ol><li id=1>One<li id=2>Two<li id=3>Three</ol>"
    val doc: Document = Jsoup.parse(h)
    val els: Elements = doc.select("#1 ~ #3")
    assert(1 == els.size)
    assert("Three" == els.first().text)
  }

  test("testCharactersInIdAndClass") {
    val h: String = "<div><p id='a1-foo_bar'>One</p><p class='b2-qux_bif'>Two</p></div>"
    val doc: Document = Jsoup.parse(h)
    val el1: Element = doc.getElementById("a1-foo_bar")
    assert("One" == el1.text)
    val el2: Element = doc.getElementsByClass("b2-qux_bif").first()
    assert("Two" == el2.text)
    val el3: Element = doc.select("#a1-foo_bar").first()
    assert("One" == el3.text)
    val el4: Element = doc.select(".b2-qux_bif").first()
    assert("Two" == el4.text)
  }

  test("testSupportsLeadingCombinator") {
    var h: String = "<div><p><span>One</span><span>Two</span></p></div>"
    var doc: Document = Jsoup.parse(h)
    val p: Element = doc.select("div > p").first()
    val spans: Elements = p.select("> span")
    assert(2 == spans.size)
    assert("One" == spans.first().text)
    h = "<div id=1><div id=2><div id=3></div></div></div>"
    doc = Jsoup.parse(h)
    val div: Element = doc.select("div").select(" > div").first()
    assert("2" == div.id)
  }

  test("testPseudoLessThan") {
    val doc: Document = Jsoup.parse("<div><p>One</p><p>Two</p><p>Three</>p></div><div><p>Four</p>")
    val ps: Elements = doc.select("div p:lt(2)")
    assert(3 == ps.size)
    assert("One" == ps.get(0).text)
    assert("Two" == ps.get(1).text)
    assert("Four" == ps.get(2).text)
  }

  test("testPseudoGreaterThan") {
    val doc: Document = Jsoup.parse("<div><p>One</p><p>Two</p><p>Three</p></div><div><p>Four</p>")
    val ps: Elements = doc.select("div p:gt(0)")
    assert(2 == ps.size)
    assert("Two" == ps.get(0).text)
    assert("Three" == ps.get(1).text)
  }

  test("testPseudoEquals") {
    val doc: Document = Jsoup.parse("<div><p>One</p><p>Two</p><p>Three</>p></div><div><p>Four</p>")
    val ps: Elements = doc.select("div p:eq(0)")
    assert(2 == ps.size)
    assert("One" == ps.get(0).text)
    assert("Four" == ps.get(1).text)
    val ps2: Elements = doc.select("div:eq(0) p:eq(0)")
    assert(1 == ps2.size)
    assert("One" == ps2.get(0).text)
    assert("p" == ps2.get(0).tagName)
  }

  test("testPseudoBetween") {
    val doc: Document = Jsoup.parse("<div><p>One</p><p>Two</p><p>Three</>p></div><div><p>Four</p>")
    val ps: Elements = doc.select("div p:gt(0):lt(2)")
    assert(1 == ps.size)
    assert("Two" == ps.get(0).text)
  }

  test("testPseudoCombined") {
    val doc: Document = Jsoup.parse("<div class='foo'><p>One</p><p>Two</p></div><div><p>Three</p><p>Four</p></div>")
    val ps: Elements = doc.select("div.foo p:gt(0)")
    assert(1 == ps.size)
    assert("Two" == ps.get(0).text)
  }

  test("testPseudoHas") {
    val doc: Document = Jsoup.parse("<div id=0><p><span>Hello</span></p></div> <div id=1><span class=foo>There</span></div> <div id=2><p>Not</p></div>")
    val divs1: Elements = doc.select("div:has(span)")
    assert(2 == divs1.size)
    assert("0" == divs1.get(0).id)
    assert("1" == divs1.get(1).id)
    val divs2: Elements = doc.select("div:has([class]")
    assert(1 == divs2.size)
    assert("1" == divs2.get(0).id)
    val divs3: Elements = doc.select("div:has(span, p)")
    assert(3 == divs3.size)
    assert("0" == divs3.get(0).id)
    assert("1" == divs3.get(1).id)
    assert("2" == divs3.get(2).id)
    val els1: Elements = doc.body.select(":has(p)")
    assert(3 == els1.size)
    assert("body" == els1.first().tagName)
    assert("0" == els1.get(1).id)
    assert("2" == els1.get(2).id)
  }

  test("testNestedHas") {
    val doc: Document = Jsoup.parse("<div><p><span>One</span></p></div> <div><p>Two</p></div>")
    var divs: Elements = doc.select("div:has(p:has(span))")
    assert(1 == divs.size)
    assert("One" == divs.first().text)
    divs = doc.select("div:has(p:matches((?i)two))")
    assert(1 == divs.size)
    assert("div" == divs.first().tagName)
    assert("Two" == divs.first().text)
    divs = doc.select("div:has(p:contains(two))")
    assert(1 == divs.size)
    assert("div" == divs.first().tagName)
    assert("Two" == divs.first().text)
  }

  test("testPseudoContains") {
    val doc: Document = Jsoup.parse("<div><p>The Rain.</p> <p class=light>The <i>rain</i>.</p> <p>Rain, the.</p></div>")
    val ps1: Elements = doc.select("p:contains(Rain)")
    assert(3 == ps1.size)
    val ps2: Elements = doc.select("p:contains(the rain)")
    assert(2 == ps2.size)
    assert("The Rain." == ps2.first().html)
    assert("The <i>rain</i>." == ps2.last.html)
    val ps3: Elements = doc.select("p:contains(the Rain):has(i)")
    assert(1 == ps3.size)
    assert("light" == ps3.first().className)
    val ps4: Elements = doc.select(".light:contains(rain)")
    assert(1 == ps4.size)
    assert("light" == ps3.first().className)
    val ps5: Elements = doc.select(":contains(rain)")
    assert(8 == ps5.size)
  }

  test("testPsuedoContainsWithParentheses") {
    val doc: Document = Jsoup.parse("<div><p id=1>This (is good)</p><p id=2>This is bad)</p>")
    val ps1: Elements = doc.select("p:contains(this (is good))")
    assert(1 == ps1.size)
    assert("1" == ps1.first().id)
    val ps2: Elements = doc.select("p:contains(this is bad\\))")
    assert(1 == ps2.size)
    assert("2" == ps2.first().id)
  }

  test("containsOwn") {
    val doc: Document = Jsoup.parse("<p id=1>Hello <b>there</b> now</p>")
    val ps: Elements = doc.select("p:containsOwn(Hello now)")
    assert(1 == ps.size)
    assert("1" == ps.first().id)
    assert(0 == doc.select("p:containsOwn(there)").size)
  }

  test("testMatches") {
    val doc: Document = Jsoup.parse("<p id=1>The <i>Rain</i></p> <p id=2>There are 99 bottles.</p> <p id=3>Harder (this)</p> <p id=4>Rain</p>")
    val p1: Elements = doc.select("p:matches(The rain)")
    assert(0 == p1.size)
    val p2: Elements = doc.select("p:matches((?i)the rain)")
    assert(1 == p2.size)
    assert("1" == p2.first().id)
    val p4: Elements = doc.select("p:matches((?i)^rain$)")
    assert(1 == p4.size)
    assert("4" == p4.first().id)
    val p5: Elements = doc.select("p:matches(\\d+)")
    assert(1 == p5.size)
    assert("2" == p5.first().id)
    val p6: Elements = doc.select("p:matches(\\w+\\s+\\(\\w+\\))")
    assert(1 == p6.size)
    assert("3" == p6.first().id)
    val p7: Elements = doc.select("p:matches((?i)the):has(i)")
    assert(1 == p7.size)
    assert("1" == p7.first().id)
  }

  test("matchesOwn") {
    val doc: Document = Jsoup.parse("<p id=1>Hello <b>there</b> now</p>")
    val p1: Elements = doc.select("p:matchesOwn((?i)hello now)")
    assert(1 == p1.size)
    assert("1" == p1.first().id)
    assert(0 == doc.select("p:matchesOwn(there)").size)
  }

  test("testRelaxedTags") {
    val doc: Document = Jsoup.parse("<abc_def id=1>Hello</abc_def> <abc-def id=2>There</abc-def>")
    val el1: Elements = doc.select("abc_def")
    assert(1 == el1.size)
    assert("1" == el1.first().id)
    val el2: Elements = doc.select("abc-def")
    assert(1 == el2.size)
    assert("2" == el2.first().id)
  }

  test("notParas") {
    val doc: Document = Jsoup.parse("<p id=1>One</p> <p>Two</p> <p><span>Three</span></p>")
    val el1: Elements = doc.select("p:not([id=1])")
    assert(2 == el1.size)
    assert("Two" == el1.first().text)
    assert("Three" == el1.last.text)
    val el2: Elements = doc.select("p:not(:has(span))")
    assert(2 == el2.size)
    assert("One" == el2.first().text)
    assert("Two" == el2.last.text)
  }

  test("notAll") {
    val doc: Document = Jsoup.parse("<p>Two</p> <p><span>Three</span></p>")
    val el1: Elements = doc.body.select(":not(p)")
    assert(2 == el1.size)
    assert("body" == el1.first().tagName)
    assert("span" == el1.last.tagName)
  }

  test("notClass") {
    val doc: Document = Jsoup.parse("<div class=left>One</div><div class=right id=1><p>Two</p></div>")
    val el1: Elements = doc.select("div:not(.left)")
    assert(1 == el1.size)
    assert("1" == el1.first().id)
  }

  test("handlesCommasInSelector") {
    val doc: Document = Jsoup.parse("<p name='1,2'>One</p><div>Two</div><ol><li>123</li><li>Text</li></ol>")
    val ps: Elements = doc.select("[name=1,2]")
    assert(1 == ps.size)
    val containers: Elements = doc.select("div, li:matches([0-9,]+)")
    assert(2 == containers.size)
    assert("div" == containers.get(0).tagName)
    assert("li" == containers.get(1).tagName)
    assert("123" == containers.get(1).text)
  }

  test("selectSupplementaryCharacter") {
    val s: String = new String(Character.toChars(135361))
    val doc: Document = Jsoup.parse("<div k" + s + "='" + s + "'>^" + s + "$/div>")
    assert("div" == doc.select("div[k" + s + "]").first().tagName)
    assert("div" == doc.select("div:containsOwn(" + s + ")").first().tagName)
  }

  test("selectClassWithSpace") {
    val html: String = "<div class=\"value\">class without space</div>\n" + "<div class=\"value \">class with space</div>"
    val doc: Document = Jsoup.parse(html)
    var found: Elements = doc.select("div[class=value ]")
    assert(2 == found.size)
    assert("class without space" == found.get(0).text)
    assert("class with space" == found.get(1).text)
    found = doc.select("div[class=\"value \"]")
    assert(2 == found.size)
    assert("class without space" == found.get(0).text)
    assert("class with space" == found.get(1).text)
    found = doc.select("div[class=\"value\\ \"]")
    assert(0 == found.size)
  }
}
