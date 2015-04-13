package com.github.ligangty.scala.jsoup.nodes

import com.github.ligangty.scala.jsoup.Jsoup
import com.github.ligangty.scala.jsoup.Connection
import org.scalatest.FunSuite

/**
 * Tests for FormElement
 *
 */
class FormElementTest extends FunSuite {

  test("hasAssociatedControls") {
    val html: String = "<form id=1><button id=1><fieldset id=2 /><input id=3><keygen id=4><object id=5><output id=6>" + "<select id=7><option></select><textarea id=8><p id=9>"
    val doc: Document = Jsoup.parse(html)
    val form: FormElement = doc.select("form").first().asInstanceOf[FormElement]
    assert(8 == form.elements.size)
  }

  test("createsFormData") {
    val html: String = "<form><input name='one' value='two'><select name='three'><option value='not'>" + "<option value='four' selected><option value='five' selected><textarea name=six>seven</textarea></form>"
    val doc: Document = Jsoup.parse(html)
    val form: FormElement = doc.select("form").first().asInstanceOf[FormElement]
    val data: Seq[Connection.KeyVal] = form.formData
    assert(4 == data.size)
    assert("one=two" == data.head.toString)
    assert("three=four" == data(1).toString)
    assert("three=five" == data(2).toString)
    assert("six=seven" == data(3).toString)
  }

  test("createsSubmitableConnection") {
    val html: String = "<form action='/search'><input name='q'></form>"
    val doc: Document = Jsoup.parse(html, "http://example.com/")
    doc.select("[name=q]").attr("value", "jsoup")
    val form: FormElement = doc.select("form").first().asInstanceOf[FormElement]
    val con: Connection = form.submit
    assert(Connection.Method.GET() == con.request.method)
    assert("http://example.com/search" == con.request.url.toExternalForm)
    val dataList: Seq[Connection.KeyVal] = con.request.data.toSeq
    assert("q=jsoup" == dataList.head.toString)
    doc.select("form").attr("method", "post")
    val con2: Connection = form.submit
    assert(Connection.Method.POST() == con2.request.method)
  }

  test("actionWithNoValue") {
    val html: String = "<form><input name='q'></form>"
    val doc: Document = Jsoup.parse(html, "http://example.com/")
    val form: FormElement = doc.select("form").first().asInstanceOf[FormElement]
    val con: Connection = form.submit
    assert("http://example.com/" == con.request.url.toExternalForm)
  }

  test("actionWithNoBaseUri") {
    val html: String = "<form><input name='q'></form>"
    val doc: Document = Jsoup.parse(html)
    val form: FormElement = doc.select("form").first().asInstanceOf[FormElement]
    var threw: Boolean = false
    try {
      form.submit
    } catch {
      case e: IllegalArgumentException =>
        threw = true
        assert("Could not determine a form action URL for submit. Ensure you set a base URI when parsing." == e.getMessage)
    }
    assert(threw)
  }

  test("formsAddedAfterParseAreFormElements") {
    val doc: Document = Jsoup.parse("<body />")
    doc.body.html("<form action='http://example.com/search'><input name='q' value='search'>")
    val formEl: Element = doc.select("form").first()
    assert(formEl.isInstanceOf[FormElement])
    val form: FormElement = formEl.asInstanceOf[FormElement]
    assert(1 == form.elements.size)
  }

  test("controlsAddedAfterParseAreLinkedWithForms") {
    val doc: Document = Jsoup.parse("<body />")
    doc.body.html("<form />")
    val formEl: Element = doc.select("form").first()
    formEl.append("<input name=foo value=bar>")
    assert(formEl.isInstanceOf[FormElement])
    val form: FormElement = formEl.asInstanceOf[FormElement]
    assert(1 == form.elements.size)
    val data: Seq[Connection.KeyVal] = form.formData
    assert("foo=bar" == data.head.toString)
  }
}
