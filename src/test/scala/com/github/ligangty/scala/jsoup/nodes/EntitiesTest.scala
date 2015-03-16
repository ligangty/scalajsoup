package com.github.ligangty.scala.jsoup.nodes

import java.lang._
import com.github.ligangty.scala.jsoup.nodes.Entities._
import org.scalatest.FunSuite

/**
 * Created by gli on 15-3-15.
 */
class EntitiesTest extends FunSuite {

  test("isBaseNamedEntity") {
    assert(isBaseNamedEntity("AElig"))
    assert(isBaseNamedEntity("yuml"))
    assert(!isBaseNamedEntity("ABC"))
  }

  test("isNamedEntity") {
    assert(isNamedEntity("AElig"))
    assert(isNamedEntity("zwnj"))
    assert(!isNamedEntity("ABC"))
  }

  test("getCharacterByName") {
    assert(Integer.parseInt("000C6", 16).toChar == getCharacterByName("AElig"))
  }

  test("Entities.EscapeMode Enumeration") {
    assert(EscapeMode.xhtml(0x00022.toChar) == "quot")
    assert(EscapeMode.base(0x000C6.toChar) == "AElig")
    assert(EscapeMode.extended(0x0200C.toChar) == "zwnj")
  }

}
