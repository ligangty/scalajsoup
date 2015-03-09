package com.github.ligangty.scala.jsoup.nodes

import java.util.Arrays

import com.github.ligangty.scala.jsoup.helper.Validator
import com.github.ligangty.scala.jsoup.helper.Validator._


/**
 *
 */
class Attribute private[Attribute]() extends java.util.Map.Entry[String, String] with Cloneable {

  private[Attribute] var key: String = null
  private[Attribute] var value: String = null

  def this(key: String, value: String) = {
    this()
    notEmpty(key)
    notNull(value)
    this.key = key
    this.value = value
  }

  /**
   * Get the attribute key.
   * @return the attribute key
   */
  override def getKey: String = key

  /**
   * Get the attribute value.
   * @return the attribute value
   */
  override def getValue: String = value

  override def setValue(value: String): String = {
    notNull(value)
    val old: String = this.value
    this.value = value
    old
  }

  //  /**
  //  Get the HTML representation of this attribute; e.g. {@code href="index.html"}.
  //     @return HTML
  //    */
  //  def html: String = {
  //    val accum: StringBuilder = new StringBuilder
  //    html(accum, (new Document("")).outputSettings)
  //    return accum.toString
  //  }
  //
  //  protected def html(accum: StringBuilder, out: Document.OutputSettings) {
  //    accum.append(key)
  //    if (!shouldCollapseAttribute(out)) {
  //      accum.append("=\"")
  //      Entities.escape(accum, value, out, true, false, false)
  //      accum.append('"')
  //    }
  //  }
  //
  //  /**
  //  Get the string representation of this attribute, implemented as {@link #html()}.
  //     @return string
  //    */
  //  override def toString: String = {
  //    return html
  //  }
  //
  //  /**
  //   * Create a new Attribute from an unencoded key and a HTML attribute encoded value.
  //   * @param unencodedKey assumes the key is not encoded, as can be only run of simple \w chars.
  //   * @param encodedValue HTML attribute encoded value
  //   * @return attribute
  //   */
  //  def createFromEncoded(unencodedKey: String, encodedValue: String): Attribute = {
  //    val value: String = Entities.unescape(encodedValue, true)
  //    return new Attribute(unencodedKey, value)
  //  }
  //
  //  protected def isDataAttribute: Boolean = {
  //    return key.startsWith(Attributes.dataPrefix) && key.length > Attributes.dataPrefix.length
  //  }
  //
  //  /**
  //   * Collapsible if it's a boolean attribute and value is empty or same as name
  //   */
  //  protected def shouldCollapseAttribute(out: Document.OutputSettings): Boolean = {
  //    return (("" == value) || value.equalsIgnoreCase(key)) && out.syntax eq Document.OutputSettings.Syntax.html && Arrays.binarySearch(booleanAttributes, key) >= 0
  //  }

  override def equals(o: Any): Boolean = o match {
    case that: Attribute =>
      this.key == that.key && this.value == that.value
    case _ => false
  }

  override def hashCode: Int = {
    var result: Int = if (key != null) key.hashCode else 0
    result = 31 * result + (if (value != null) value.hashCode else 0)
    return result
  }

  override def clone: Attribute = {
    try {
      return super.clone.asInstanceOf[Attribute]
    }
    catch {
      case e: CloneNotSupportedException => {
        throw new RuntimeException(e)
      }
    }
  }
}

object Attribute {
  private val booleanAttributes: Array[String] = Array("allowfullscreen", "async", "autofocus", "checked", "compact", "declare", "default", "defer", "disabled", "formnovalidate", "hidden", "inert", "ismap", "itemscope", "multiple", "muted", "nohref", "noresize", "noshade", "novalidate", "nowrap", "open", "readonly", "required", "reversed", "seamless", "selected", "sortable", "truespeed", "typemustmatch")

  def apply(key: String, value: String): Attribute = new Attribute(key.trim.toLowerCase, value)

}