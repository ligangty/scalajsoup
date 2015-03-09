package com.github.ligangty.scala.jsoup.parser

import org.scalatest.FunSuite

/**
 * Created by gli on 3/9/15.
 */
class TagTest extends FunSuite {
  test("case sensitive") {
    val p1: Tag = Tag("P")
    val p2: Tag = Tag("p")
    assert(p1 == p2)
  }

  test("trims") {
    val p1: Tag = Tag("p")
    val p2: Tag = Tag(" p ")
    assertEquals(p1, p2)
  }

  test("equality"){
    val p1: Tag = Tag("p")
    val p2: Tag = Tag("p")
    assertTrue(p1.equals(p2))
    assertTrue(p1 == p2)
  }

  test("divSemantics"){
    val div = Tag("div")

    assertTrue(div.isBlock)
    assertTrue(div.isFormatAsBlock)
  }

  test("pSemantics"){
    val p = Tag("p")

    assertTrue(p.isBlock)
    assertFalse(p.isFormatAsBlock)
  }

  test("imgSemantics"){
    val img = Tag("img")
    assertTrue(img.isInline)
    assertTrue(img.isSelfClosing)
    assertFalse(img.isBlock)
  }

  test("defaultSemantics"){
    val foo = Tag("foo") // not defined
    val foo2 = Tag("FOO")

    assertEquals(foo, foo2)
    assertTrue(foo.isInline)
    assertTrue(foo.isFormatAsBlock)
  }

  intercept[IllegalArgumentException]{
    Tag(null)
  }

  intercept[IllegalArgumentException]{
    Tag(" ")
  }
}
