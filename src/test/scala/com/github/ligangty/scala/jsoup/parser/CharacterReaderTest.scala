package com.github.ligangty.scala.jsoup.parser

import org.scalatest.FunSuite

/**
 * Test suite for character reader.
 */
class CharacterReaderTest extends FunSuite {

  test("consume") {
    val r: CharacterReader = new CharacterReader("one")
    assert(0 == r.pos)
    assert('o' == r.current)
    assert('o' == r.consume)
    assert(1 == r.pos)
    assert('n' == r.current)
    assert(1 == r.pos)
    assert('n' == r.consume)
    assert('e' == r.consume)
    assert(r.isEmpty)
    assert(CharacterReader.EOF == r.consume)
    assert(r.isEmpty)
    assert(CharacterReader.EOF == r.consume)
  }

  test("unconsume") {
    val r: CharacterReader = new CharacterReader("one")
    assert('o' == r.consume)
    assert('n' == r.current)
    r.unconsume()
    assert('o' == r.current)
    assert('o' == r.consume)
    assert('n' == r.consume)
    assert('e' == r.consume)
    assert(r.isEmpty)
    r.unconsume()
    assert(!r.isEmpty)
    assert('e' == r.current)
    assert('e' == r.consume)
    assert(r.isEmpty)
    assert(CharacterReader.EOF == r.consume)
    r.unconsume()
    assert(r.isEmpty)
    assert(CharacterReader.EOF == r.current)
  }

  test("mark") {
    val r: CharacterReader = new CharacterReader("one")
    r.consume
    r.mark()
    assert('n' == r.consume)
    assert('e' == r.consume)
    assert(r.isEmpty)
    r.rewindToMark()
    assert('n' == r.consume)
  }

  test("consumeToEnd") {
    val in: String = "one two three"
    val r: CharacterReader = new CharacterReader(in)
    val toEnd: String = r.consumeToEnd
    assert(in == toEnd)
    assert(r.isEmpty)
  }

  test("nextIndexOfChar") {
    val in: String = "blah blah"
    val r: CharacterReader = new CharacterReader(in)
    assert(-1 == r.nextIndexOf('x'))
    assert(3 == r.nextIndexOf('h'))
    val pull: String = r.consumeTo('h')
    assert("bla" == pull)
    r.consume
    assert(2 == r.nextIndexOf('l'))
    assert(" blah" == r.consumeToEnd)
    assert(-1 == r.nextIndexOf('x'))
  }

  test("nextIndexOfString") {
    val in: String = "One Two something Two Three Four"
    val r: CharacterReader = new CharacterReader(in)
    assert(-1 == r.nextIndexOf("Foo"))
    assert(4 == r.nextIndexOf("Two"))
    assert("One Two " == r.consumeTo("something"))
    assert(10 == r.nextIndexOf("Two"))
    assert("something Two Three Four" == r.consumeToEnd)
    assert(-1 == r.nextIndexOf("Two"))
  }

  test("nextIndexOfUnmatched") {
    val r: CharacterReader = new CharacterReader("<[[one]]")
    assert(-1 == r.nextIndexOf("]]>"))
  }

  test("consumeToChar") {
    val r: CharacterReader = new CharacterReader("One Two Three")
    assert("One " == r.consumeTo('T'))
    assert("" == r.consumeTo('T'))
    assert('T' == r.consume)
    assert("wo " == r.consumeTo('T'))
    assert('T' == r.consume)
    assert("hree" == r.consumeTo('T'))
  }

  test("consumeToString") {
    val r: CharacterReader = new CharacterReader("One Two Two Four")
    assert("One " == r.consumeTo("Two"))
    assert('T' == r.consume)
    assert("wo " == r.consumeTo("Two"))
    assert('T' == r.consume)
    assert("wo Four" == r.consumeTo("Qux"))
  }

  test("advance") {
    val r: CharacterReader = new CharacterReader("One Two Three")
    assert('O' == r.consume)
    r.advance()
    assert('e' == r.consume)
  }

  test("consumeToAny") {
    val r: CharacterReader = new CharacterReader("One &bar; qux")
    assert("One " == r.consumeToAny('&', ';'))
    assert(r.matches('&'))
    assert(r.matches("&bar;"))
    assert('&' == r.consume)
    assert("bar" == r.consumeToAny('&', ';'))
    assert(';' == r.consume)
    assert(" qux" == r.consumeToAny('&', ';'))
  }

  test("consumeLetterSequence") {
    val r: CharacterReader = new CharacterReader("One &bar; qux")
    assert("One" == r.consumeLetterSequence)
    assert(" &" == r.consumeTo("bar;"))
    assert("bar" == r.consumeLetterSequence)
    assert("; qux" == r.consumeToEnd)
  }

  test("consumeLetterThenDigitSequence") {
    val r: CharacterReader = new CharacterReader("One12 Two &bar; qux")
    assert("One12" == r.consumeLetterThenDigitSequence)
    assert(' ' == r.consume)
    assert("Two" == r.consumeLetterThenDigitSequence)
    assert(" &bar; qux" == r.consumeToEnd)
  }

  test("matches") {
    val r: CharacterReader = new CharacterReader("One Two Three")
    assert(r.matches('O'))
    assert(r.matches("One Two Three"))
    assert(r.matches("One"))
    assert(!r.matches("one"))
    assert('O' == r.consume)
    assert(!r.matches("One"))
    assert(r.matches("ne Two Three"))
    assert(!r.matches("ne Two Three Four"))
    assert("ne Two Three" == r.consumeToEnd)
    assert(!r.matches("ne"))
  }

  test("matchesIgnoreCase") {
    val r: CharacterReader = new CharacterReader("One Two Three")
    assert(r.matchesIgnoreCase("O"))
    assert(r.matchesIgnoreCase("o"))
    assert(r.matches('O'))
    assert(!r.matches('o'))
    assert(r.matchesIgnoreCase("One Two Three"))
    assert(r.matchesIgnoreCase("ONE two THREE"))
    assert(r.matchesIgnoreCase("One"))
    assert(r.matchesIgnoreCase("one"))
    assert('O' == r.consume)
    assert(!r.matchesIgnoreCase("One"))
    assert(r.matchesIgnoreCase("NE Two Three"))
    assert(!r.matchesIgnoreCase("ne Two Three Four"))
    assert("ne Two Three" == r.consumeToEnd)
    assert(!r.matchesIgnoreCase("ne"))
  }

  test("containsIgnoreCase") {
    val r: CharacterReader = new CharacterReader("One TWO three")
    assert(r.containsIgnoreCase("two"))
    assert(r.containsIgnoreCase("three"))
    assert(!r.containsIgnoreCase("one"))
  }

  test("matchesAny") {
    val scan: Array[Char] = Array(' ', '\n', '\t')
    val r: CharacterReader = new CharacterReader("One\nTwo\tThree")
    assert(!r.matchesAny(scan))
    assert("One" == r.consumeToAny(scan))
    assert(r.matchesAny(scan))
    assert('\n' == r.consume)
    assert(!r.matchesAny(scan))
  }

  test("cachesStrings") {
    val r: CharacterReader = new CharacterReader("Check\tCheck\tCheck\tCHOKE\tA string that is longer than 16 chars")
    val one: String = r.consumeTo('\t')
    r.consume
    val two: String = r.consumeTo('\t')
    r.consume
    val three: String = r.consumeTo('\t')
    r.consume
    val four: String = r.consumeTo('\t')
    r.consume
    val five: String = r.consumeTo('\t')
    assert("Check" == one)
    assert("Check" == two)
    assert("Check" == three)
    assert("CHOKE" == four)
    assert(one eq two)
    assert(two eq three)
    assert(three ne four)
    assert(four ne five)
    assert(five == "A string that is longer than 16 chars")
  }

  test("rangeEquals") {
    val r: CharacterReader = new CharacterReader("Check\tCheck\tCheck\tCHOKE")
    assert(r.rangeEquals(0, 5, "Check"))
    assert(!r.rangeEquals(0, 5, "CHOKE"))
    assert(!r.rangeEquals(0, 5, "Chec"))
    assert(r.rangeEquals(6, 5, "Check"))
    assert(!r.rangeEquals(6, 5, "Chuck"))
    assert(r.rangeEquals(12, 5, "Check"))
    assert(!r.rangeEquals(12, 5, "Cheeky"))
    assert(r.rangeEquals(18, 5, "CHOKE"))
    assert(!r.rangeEquals(18, 5, "CHIKE"))
  }
}
