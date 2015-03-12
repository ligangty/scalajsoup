package com.github.ligangty.scala.jsoup.nodes

import com.github.ligangty.scala.jsoup.helper.Validator._
import com.github.ligangty.scala.jsoup.parser.Tag

/**
 * Created by gli on 15-3-10.
 */
class Element(baseUri: String, attributes: Attributes) extends Node(baseUri, attributes) {
  private var tag: Tag = null

  /**
   * Create a new, standalone Element. (Standalone in that is has no parent.)
   *
   * @param tag tag of this element
   * @param baseUri the base URI
   * @param attributes initial attributes
   * @see #appendChild(Node)
   * @see #appendElement(String)
   */
  def this(tag: Tag, baseUri: String, attributes: Attributes) {
    this(baseUri, attributes)
    notNull(tag)
    this.tag = tag
  }

  /**
   * Create a new Element from a tag and a base URI.
   *
   * @param tag element tag
   * @param baseUri the base URI of this element. It is acceptable for the base URI to be an empty
   *                string, but not null.
   * @see Tag#valueOf(String)
   */
  def this(tag: Tag, baseUri: String) {
    this(tag, baseUri, new Attributes)
  }
}
