package com.github.ligangty.scala.jsoup.nodes

import com.github.ligangty.scala.jsoup.parser.Tag


/**
 * Created by gli on 3/9/15.
 */
abstract class Node extends scala.Cloneable  {
  private[nodes] var parentNode: Node = null
  private[nodes] var childNodes: List[Node] = null
  private[nodes] var attributes: Attributes = null
  private[nodes] var baseUri: String = null
  private[nodes] var siblingIndex: Int = 0
}
