package com.github.ligangty.scala.jsoup.nodes

import org.scalatest.FunSuite

/**
 * Created by gli on 15-3-9.
 */
class AttributeTest extends FunSuite {
  test("html") {
    val attr: Attribute = new Attribute("key", "value &")
//    assert("key=\"value &amp;\"" == attr.html)
//    assert(attr.html == attr.toString)
  }

  test("Supplementary Character should In AttributeKeyAndValue") {
    val s: String = new String(Character.toChars(135361))
    val attr: Attribute = new Attribute(s, "A" + s + "B")
//    assert(s + "=\"A" + s + "B\"" == attr.html)
//    assert(attr.html == attr.toString)
  }
}
