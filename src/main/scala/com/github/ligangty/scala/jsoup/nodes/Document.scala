package com.github.ligangty.scala.jsoup.nodes

import com.github.ligangty.scala.jsoup.parser.Tag

/**
 * Created by gli on 15-3-10.
 */
class Document private(baseUri: String, location: String) extends Element(Tag("#root"), baseUri) {
  def this(baseUri: String) {
    this(baseUri, baseUri)
  }
}
