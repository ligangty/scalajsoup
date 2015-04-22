package com.github.ligangty.scala.jsoup.nodes

import DataNode._

/**
 * A data node, for contents of style, script tags etc, where contents should not show in text().
 */
class DataNode(baseUri: String) extends Node(baseUri) {
  /**
   * Create a new DataNode.
   * @param data data contents
   * @param baseUri base URI
   */
  def this(data: String, baseUri: String) {
    this(baseUri)
    attributesVal.put(DATA_KEY, data)
  }

  override def nodeName(): String = "#data"

  /**
   * Get the data contents of this node. Will be unescaped and with original new lines, space etc.
   * @return data
   */
  def getWholeData: String = attributesVal.get(DATA_KEY)

  /**
   * Set the data contents of this node.
   * @param data unencoded data
   * @return this node, for chaining
   */
  def setWholeData(data: String): DataNode = {
    attributesVal.put(DATA_KEY, data)
    this
  }

  private[nodes] def outerHtmlHead(accum: StringBuilder, depth: Int, out: Document.OutputSettings): Unit =
    accum.append(getWholeData)

  private[nodes] def outerHtmlTail(accum: StringBuilder, depth: Int, out: Document.OutputSettings): Unit = ()

  override def toString: String = outerHtml
}

object DataNode {
  private val DATA_KEY: String = "data"

  /**
   * Create a new DataNode from HTML encoded data.
   * @param encodedData encoded data
   * @param baseUri bass URI
   * @return new DataNode
   */
  def createFromEncoded(encodedData: String, baseUri: String): DataNode = {
    val data: String = Entities.unescape(encodedData)
    new DataNode(data, baseUri)
  }
}
