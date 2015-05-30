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
  def getWholeDeclaration: String = {
    val decl: String = attributesVal.get(DECL_KEY)
    if (decl == "xml" && attributesVal.size > 1) {
      val sb: StringBuilder = new StringBuilder(decl)
      val version: String = attributesVal.get("version")
      if (version != null) {
        sb.append(" version=\"").append(version).append("\"")
      }
      val encoding: String = attributesVal.get("encoding")
      if (encoding != null) {
        sb.append(" encoding=\"").append(encoding).append("\"")
      }
      sb.toString()
    } else {
      attributesVal.get(DECL_KEY)
    }
  }

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

  private[nodes] val DECL_KEY: String = "declaration"
}
