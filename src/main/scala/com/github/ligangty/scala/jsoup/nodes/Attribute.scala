package com.github.ligangty.scala.jsoup.nodes

import java.util

import com.github.ligangty.scala.jsoup.helper.Validator._

/**
 *
 */
class Attribute(var key: String, var value: String) extends Product2[String, String] with Cloneable {
  notEmpty(key)
  notNull(value)
  key = key.toLowerCase.trim

  override def _1 = key

  override def _2 = value


  /**
   * Get the attribute key.
   * @return the attribute key
   */
  def getKey: String = key

  /**
   * Get the attribute value.
   * @return the attribute value
   */
  def getValue: String = value

  def setValue(value: String): String = {
    notNull(value)
    val old: String = this.value
    this.value = value
    old
  }

  /**
  Get the HTML representation of this attribute; e.g. <code>href="index.html"</code>.
       @return HTML
    */
  def html: String = {
    val accum: StringBuilder = new StringBuilder
    html(accum, new Document("").outputSettings)
    accum.toString()
  }

  private[nodes] def html(accum: StringBuilder, out: Document.OutputSettings) {
    accum.append(key)
    if (!shouldCollapseAttribute(out)) {
      accum.append("=\"")
      Entities.escape(accum, value, out, true, false, false)
      accum.append('"')
    }
  }

  /**
   * Get the string representation of this attribute, implemented as [[html()]].
   * @return string
   */
  override def toString: String = html


  /**
   * Create a new Attribute from an unencoded key and a HTML attribute encoded value.
   * @param unencodedKey assumes the key is not encoded, as can be only run of simple \w chars.
   * @param encodedValue HTML attribute encoded value
   * @return attribute
   */
  def createFromEncoded(unencodedKey: String, encodedValue: String): Attribute = {
    val value: String = Entities.unescape(encodedValue, true)
    new Attribute(unencodedKey, value)
  }


  private[nodes] def isDataAttribute: Boolean =
    key.startsWith(Attributes.dataPrefix) && key.length > Attributes.dataPrefix.length

  /**
   * Collapsible if it's a boolean attribute and value is empty or same as name
   */
  private[nodes] def shouldCollapseAttribute(out: Document.OutputSettings): Boolean = {
    (("" == value) || value.equalsIgnoreCase(key)) &&
      (out.syntax == Document.OutputSettings.Syntax.html) &&
      (util.Arrays.binarySearch(Attribute.booleanAttributes.asInstanceOf[Array[AnyRef]], key) >= 0)
  }

  override def equals(o: Any): Boolean = o match {
    case that: Attribute if this eq that => true
    case that: Attribute if this.key != null && this.key != that.key => false
    case that: Attribute if this.key == null && that.key != null => false
    case that: Attribute if this.value != null && this.value != that.value => false
    case that: Attribute if this.value == null && that.value != null => false
    case that: Attribute => true
    case _ => false
  }

  override def hashCode: Int = {
    var result: Int = if (key != null) {
      key.##
    } else {
      0
    }
    result = 31 * result + (if (value != null) {
      value.##
    } else {
      0
    })
    result
  }

  override def clone: Attribute = {
    try {
      super.clone.asInstanceOf[Attribute] // only fields are immutable strings key and value, so no more deep copy required
    }
    catch {
      case e: CloneNotSupportedException => throw new RuntimeException(e)
    }
  }

  override def canEqual(that: Any): Boolean = that.isInstanceOf[Attribute]
}

object Attribute {

  private val booleanAttributes: Array[String] = Array("allowfullscreen", "async", "autofocus", "checked", "compact", "declare", "default", "defer", "disabled", "formnovalidate", "hidden", "inert", "ismap", "itemscope", "multiple", "muted", "nohref", "noresize", "noshade", "novalidate", "nowrap", "open", "readonly", "required", "reversed", "seamless", "selected", "sortable", "truespeed", "typemustmatch")

  def apply(key: String, value: String): Attribute = new Attribute(key.trim.toLowerCase, value)

  implicit def attributeToTuple2(attr: Attribute): (String, String) = (attr._1, attr._2)
}