package com.github.ligangty.scala.jsoup.nodes

import com.github.ligangty.scala.jsoup.parser.Tag


/**
 * Created by gli on 3/9/15.
 */
abstract class Node protected(private var baseUri: String, private var attributes: Attributes) extends scala.Cloneable {
  private[nodes] var parentNode: Node = null
  private[nodes] var childNodes: List[Node] = null
  private[nodes] var siblingIndex: Int = 0

  def nodeName():String
}
