package com.github.ligangty.scala.jsoup.helper

import org.scalatest.FunSuite

/**
 * Created by gli on 3/11/15.
 */
class StringsTest extends FunSuite {
  test("join Iterable") {
    assert("" == Strings.join(List(""), " "))
    assert("one" == Strings.join(List("one"), " "))
    assert("one two three" == Strings.join(List("one", "two", "three"), " "))
  }

  test("join Array") {
    assert("" == Strings.join(Array(""), " "))
    assert("one" == Strings.join(Array("one"), " "))
    assert("one two three" == Strings.join(Array("one", "two", "three"), " "))
  }

  test("padding") {
    assert("" == Strings.padding(0))
    assert(" " == Strings.padding(1))
    assert("  " == Strings.padding(2))
    assert("               " == Strings.padding(15))
  }
}
