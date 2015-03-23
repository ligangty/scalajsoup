package com.github.ligangty.scala.jsoup.nodes

import org.scalatest.FunSuite

/**
 * Created by gli on 3/19/15.
 */
class AttributesTest extends FunSuite {
  test("html") {
    val a: Attributes = new Attributes
    a.put("Tot", "a&p")
    a.put("Hello", "There")
    a.put("data-name", "Jsoup")
    assert(3 == a.size)
    assert(a.hasKey("tot"))
    assert(a.hasKey("Hello"))
    assert(a.hasKey("data-name"))
    assert("a&p" == a.get("tot"))
    assert("" == a.get("notfound"))
    val list = a.asList
    assert(3 == list.size)
    assert(list.contains(new Attribute("Tot", "a&p")))
    assert(list.contains(new Attribute("Hello", "There")))
    assert(list.contains(new Attribute("data-name", "Jsoup")))
    assert(1 == a.dataset.size)
    assert("Jsoup" == a.dataset("name"))
    assert(" tot=\"a&amp;p\" hello=\"There\" data-name=\"Jsoup\"" == a.html)
    assert(a.html == a.toString)
  }
}
