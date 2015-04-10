package com.github.ligangty.scala.jsoup.parser

import org.scalatest.FunSuite

/**
 * Token queue tests.
 */
class TokenQueueTest extends FunSuite {

  test("chompBalanced") {
    val tq: TokenQueue = new TokenQueue(":contains(one (two) three) four")
    val pre: String = tq.consumeTo("(")
    val guts: String = tq.chompBalanced('(', ')')
    val remainder: String = tq.remainder
    assert(":contains" == pre)
    assert("one (two) three" == guts)
    assert(" four" == remainder)
  }

  test("chompEscapedBalanced") {
    val tq: TokenQueue = new TokenQueue(":contains(one (two) \\( \\) \\) three) four")
    val pre: String = tq.consumeTo("(")
    val guts: String = tq.chompBalanced('(', ')')
    val remainder: String = tq.remainder
    assert(":contains" == pre)
    assert("one (two) \\( \\) \\) three" == guts)
    assert("one (two) ( ) ) three" == TokenQueue.unescape(guts))
    assert(" four" == remainder)
  }

  test("chompBalancedMatchesAsMuchAsPossible") {
    val tq: TokenQueue = new TokenQueue("unbalanced(something(or another")
    tq.consumeTo("(")
    val matched: String = tq.chompBalanced('(', ')')
    assert("something(or another" == matched)
  }

  test("unescape") {
    assert("one ( ) \\" == TokenQueue.unescape("one \\( \\) \\\\"))
  }

  test("chompToIgnoreCase") {
    val t: String = "<textarea>one < two </TEXTarea>"
    var tq: TokenQueue = new TokenQueue(t)
    var data: String = tq.chompToIgnoreCase("</textarea")
    assert("<textarea>one < two " == data)
    tq = new TokenQueue("<textarea> one two < three </oops>")
    data = tq.chompToIgnoreCase("</textarea")
    assert("<textarea> one two < three </oops>" == data)
  }

  test("addFirst") {
    val tq: TokenQueue = new TokenQueue("One Two")
    tq.consumeWord
    tq.addFirst("Three")
    assert("Three Two" == tq.remainder)
  }
}
