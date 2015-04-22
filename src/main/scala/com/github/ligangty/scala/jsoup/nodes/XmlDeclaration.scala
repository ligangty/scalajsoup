package com.github.ligangty.scala.jsoup.nodes

import XmlDeclaration._

/**
 * An XML Declaration.
 */
class XmlDeclaration(baseUri: String, isProcessingInstruction: Boolean) extends Node(baseUri) {

  /**
   * Create a new XML declaration
   * @param data data
   * @param baseUri base uri
   * @param isProcessingInstruction is processing instruction
   */
  def this(data: String, baseUri: String, isProcessingInstruction: Boolean) {
    this(baseUri, isProcessingInstruction)
    attributesVal.put(DECL_KEY, data)
  }

  override def nodeName(): String = "#declaration"

  /**
   * Get the unencoded XML declaration.
   * @return XML declaration
   */
  def getWholeDeclaration: String = attributesVal.get(DECL_KEY)

  private[nodes] def outerHtmlHead(accum: StringBuilder, depth: Int, out: Document.OutputSettings) {
    accum.append("<").append(if (isProcessingInstruction) {
      "!"
    } else {
      "?"
    }).append(getWholeDeclaration).append(">")
  }

  private[nodes] def outerHtmlTail(accum: StringBuilder, depth: Int, out: Document.OutputSettings) {}

  override def toString: String = outerHtml
}

private[nodes] object XmlDeclaration {

  private val DECL_KEY: String = "declaration"
}
