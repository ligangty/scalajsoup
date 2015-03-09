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
    assert(p1 == p2)
  }

  test("equality") {
    val p1: Tag = Tag("p")
    val p2: Tag = Tag("p")
    assert(p1.equals(p2))
    assert(p1 == p2)
  }

  test("divSemantics") {
    val div = Tag("div")

    assert(div.isBlock)
    assert(div.isFormatAsBlock)
  }

  test("pSemantics") {
    val p = Tag("p")

    assert(p.isBlock)
    assert(!p.isFormatAsBlock)
  }

  test("imgSemantics") {
    val img = Tag("img")
    assert(img.isInline)
    assert(img.isSelfClosing)
    assert(!img.isBlock)
  }

  test("defaultSemantics") {
    val foo = Tag("foo") // not defined
    val foo2 = Tag("FOO")

    assert(foo == foo2)
    assert(foo.isInline)
    assert(foo.isFormatAsBlock)
  }

  test("null"){
    intercept[IllegalArgumentException] {
      Tag(null)
    }
  }

  test("empty"){
    intercept[IllegalArgumentException] {
      Tag(" ")
    }
  }
}
