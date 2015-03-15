package com.github.ligangty.scala.jsoup.nodes

import org.scalatest.FunSuite

/**
 * Created by gli on 15-3-15.
 */
class EntitiesTest extends FunSuite {
  test("isBaseNamedEntity") {
    assert(Entities.isBaseNamedEntity("AElig"))
    assert(Entities.isBaseNamedEntity("yuml"))
    assert(!Entities.isBaseNamedEntity("ABC"))
  }

  test("isNamedEntity") {
    assert(Entities.isNamedEntity("AElig"))
    assert(Entities.isNamedEntity("zwnj"))
    assert(!Entities.isNamedEntity("ABC"))
  }
}
