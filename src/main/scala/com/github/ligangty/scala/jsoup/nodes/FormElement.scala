package com.github.ligangty.scala.jsoup.nodes

import com.github.ligangty.scala.jsoup.{Jsoup, Connection}
import com.github.ligangty.scala.jsoup.helper.{HttpConnection, Validator}
import com.github.ligangty.scala.jsoup.parser.Tag
import com.github.ligangty.scala.jsoup.select.Elements

import scala.collection.mutable.ArrayBuffer

/**
 * A HTML Form Element provides ready access to the form fields/controls that are associated with it. It also allows a
 * form to easily be submitted.
 */
class FormElement(tag: Tag, baseUri: String, attributes: Attributes) extends Element(tag, baseUri, attributes) {

  private val elems: Elements = new Elements

  /**
   * Get the list of form control elems associated with this form.
   * @return form controls associated with this element.
   */
  def elements: Elements = {
    elems
  }

  /**
   * Add a form control element to this form.
   * @param element form control to add
   * @return this form element, for chaining
   */
  def addElement(element: Element): FormElement = {
    elems.add(element)
    this
  }

  /**
   * Prepare to submit this form. A Connection object is created with the request set up from the form values. You
   * can then set up other options (like user-agent, timeout, cookies), then execute it.
   * @return a connection prepared from the values of this form.
   * @throws IllegalArgumentException if the form's absolute action URL cannot be determined. Make sure you pass the
   *                                  document's base URI when parsing.
   */
  def submit: Connection = {
    val action: String = if (hasAttr("action")) {
      absUrl("action")
    } else {
      baseUri
    }
    Validator.notEmpty(action, "Could not determine a form action URL for submit. Ensure you set a base URI when parsing.")
    val method: Connection.Method.Method = if (attr("method").toUpperCase == "POST") {
      Connection.Method.POST()
    } else {
      Connection.Method.GET()
    }
    val con: Connection = Jsoup.connect(action).data(formData).method(method)
    con
  }

  /**
   * Get the data that this form submits. The returned list is a copy of the data, and changes to the contents of the
   * list will not be reflected in the DOM.
   * @return a list of key vals
   */
  def formData: Seq[Connection.KeyVal] = {
    val data: ArrayBuffer[Connection.KeyVal] = new ArrayBuffer[Connection.KeyVal]
    import scala.util.control.Breaks._
    // iterate the form control elements and accumulate their values
    for (el <- elems) {
      breakable {
        // contents are form listable, superset of submitable
        if (!el.tag.isFormSubmittable) {
          break()
        }
        // skip disabled form inputs
        if (el.hasAttr("disabled")) {
          break()
        }
        val name: String = el.attr("name")
        if (name.length == 0) {
          break()
        }

        val typeVal: String = el.attr("type")
        if ("select" == el.tagName) {
          val options: Elements = el.select("option[selected]")
          var set: Boolean = false
          for (option <- options) {
            data.append(HttpConnection.KeyVal.create(name, option.value))
            set = true
          }
          if (!set) {
            val option: Element = el.select("option").first()
            if (option != null) {
              data.append(HttpConnection.KeyVal.create(name, option.value))
            }
          }
        } else if ("checkbox".equalsIgnoreCase(typeVal) || "radio".equalsIgnoreCase(typeVal)) {
          // only add checkbox or radio if they have the checked attribute
          if (el.hasAttr("checked")) {
            val value = if (el.value.length() > 0) {
              el.value
            } else {
              "on"
            }
            data.append(HttpConnection.KeyVal.create(name, value) )
          }
        } else {
          data.append(HttpConnection.KeyVal.create(name, el.value))
        }
      }
    }
    data.toSeq
  }

  override def equals(o: Any): Boolean = {
    super.equals(o)
  }
}
