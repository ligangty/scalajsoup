package com.github.ligangty.scala.jsoup.helper

import com.github.ligangty.scala.jsoup.helper.Strings._
import org.scalatest.FunSuite


/**
 * Created by gli on 3/11/15.
 */
class StringsTest extends FunSuite {
  test("join Iterable") {
    assertResult("")(join(List(""), " "))
    assertResult("one")(join(List("one"), " "))
    assertResult("one two three")(join(List("one", "two", "three"), " "))
  }

  test("join Array") {
    assertResult("")(join(Array(""), " "))
    assertResult("one")(join(Array("one"), " "))
    assertResult("one two three")(join(Array("one", "two", "three"), " "))
  }

  test("padding") {
    assertResult("")(padding(0))
    assertResult(" ")(padding(1))
    assertResult("  ")(padding(2))
    assertResult("               ")(padding(15))
  }

  test("isWhitespace") {
    assert(isWhitespace('\t'))
    assert(isWhitespace('\n'))
    assert(isWhitespace('\r'))
    assert(isWhitespace('\f'))
    assert(isWhitespace(' '))
    assert(!isWhitespace('\u00a0'))
    assert(!isWhitespace('\u2000'))
    assert(!isWhitespace('\u3000'))
  }

  test("isBlank") {
    assert(isBlank(null))
    assert(isBlank(""))
    assert(isBlank("      "))
    assert(isBlank("   \r\n  "))

    assert(!isBlank("hello"))
    assert(!isBlank("   hello   "))
  }

  test("isNumeric") {
    assert(!isNumeric(null))
    assert(!isNumeric(" "))
    assert(!isNumeric("123 546"))
    assert(!isNumeric("hello"))
    assert(!isNumeric("123.334"))

    assert(isNumeric("1"))
    assert(isNumeric("1234"))
  }

  test("normaliseWhiteSpace") {
    assertResult(" ")(normaliseWhitespace("    \r \n \r\n"))
    assertResult(" hello there ")(normaliseWhitespace("   hello   \r \n  there    \n"))
    assertResult("hello")(normaliseWhitespace("hello"))
    assertResult("hello there")(normaliseWhitespace("hello\nthere"))
  }

 test("normaliseWhiteSpaceHandlesHighSurrogates") {
    val test71540chars = "\ud869\udeb2\u304b\u309a  1"
    val test71540charsExpectedSingleWhitespace = "\ud869\udeb2\u304b\u309a 1"

    assertResult(test71540charsExpectedSingleWhitespace)(normaliseWhitespace(test71540chars))
//    val extractedText = Jsoup.parse(test71540chars).text()
//    assertResult(test71540charsExpectedSingleWhitespace)(extractedText)
  }
}
