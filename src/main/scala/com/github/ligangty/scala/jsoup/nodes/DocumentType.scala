package com.github.ligangty.scala.jsoup.nodes

import com.github.ligangty.scala.jsoup.helper.Strings
import DocumentType._

/**
 * A {@code <!DOCTYPE>} node.
 */
class DocumentType(baseUri: String) extends Node(baseUri){
  /**
   * Create a new doctype element.
   * @param name the doctype's name
   * @param publicId the doctype's public ID
   * @param systemId the doctype's system ID
   * @param baseUri the doctype's base URI
   */
  def this(name: String, publicId: String, systemId: String, baseUri: String) {
    this(baseUri)
    attr(NAME, name)
    attr(PUBLIC_ID, publicId)
    attr(SYSTEM_ID, systemId)
  }

  override def nodeName(): String =  "#doctype"

  override private[nodes] def outerHtmlHead(accum: StringBuilder, depth: Int, out: Document.OutputSettings) {
    if (out.syntax == Document.OutputSettings.Syntax.html && !has(PUBLIC_ID) && !has(SYSTEM_ID)) {
      accum.append("<!doctype")
    } else {
      accum.append("<!DOCTYPE")
    }
    if (has(NAME)) accum.append(" ").append(attr(NAME))
    if (has(PUBLIC_ID)) accum.append(" PUBLIC \"").append(attr(PUBLIC_ID)).append('"')
    if (has(SYSTEM_ID)) accum.append(" \"").append(attr(SYSTEM_ID)).append('"')
    accum.append('>')
  }

  override private[nodes] def outerHtmlTail(accum: StringBuilder, depth: Int, out: Document.OutputSettings) {}

  private def has(attribute: String): Boolean =  !Strings.isBlank(attr(attribute))

}

private[nodes] object DocumentType{
  private val NAME: String = "name"
  private val PUBLIC_ID: String = "publicId"
  private val SYSTEM_ID: String = "systemId"
  // todo: quirk mode from publicId and systemId
}
