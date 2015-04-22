package com.github.ligangty.scala.jsoup.nodes

/**
 * A comment node.
 */
class Comment(baseUri: String) extends Node(baseUri) {
  private val COMMENT_KEY: String = "comment"

  /**
   * Create a new comment node.
   * @param data The contents of the comment
   * @param baseUri base URI
   */
  def this(data: String, baseUri: String) {
    this(baseUri)
    attributesVal.put(COMMENT_KEY, data)
  }

  def nodeName(): String = "#comment"

  /**
   * Get the contents of the comment.
   * @return comment content
   */
  def getData: String = attributes.get(COMMENT_KEY)

  private[nodes] def outerHtmlHead(accum: StringBuilder, depth: Int, out: Document.OutputSettings) {
    if (out.prettyPrint) indent(accum, depth, out)
    accum.append("<!--").append(getData).append("-->")
  }

  private[nodes] def outerHtmlTail(accum: StringBuilder, depth: Int, out: Document.OutputSettings) {}

  override def toString: String = outerHtml
}
