package com.github.ligangty.scala.jsoup.helper

import java.io.StringWriter
import javax.xml.parsers.{ParserConfigurationException, DocumentBuilder, DocumentBuilderFactory}
import javax.xml.transform.{Transformer, TransformerException, TransformerFactory}
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult

import com.github.ligangty.scala.jsoup.nodes
import com.github.ligangty.scala.jsoup.select.{NodeVisitor, NodeTraversor}
import org.w3c.dom

/**
 * Helper class to transform a [[com.github.ligangty.scala.jsoup.nodes.Document]] to a [[org.w3c.dom.Document]],
 * for integration with toolsets that use the W3C DOM.
 * <p/>
 * This class is currently <b>experimental</b>, please provide feedback on utility and any problems experienced.
 */
class W3CDom {

  protected var factory: DocumentBuilderFactory = DocumentBuilderFactory.newInstance

  /**
   * Convert a jsoup Document to a W3C Document.
   * @param in jsoup doc
   * @return w3c doc
   */
  def fromJsoup(in: nodes.Document): dom.Document = {
    Validator.notNull(in)
    var builder: DocumentBuilder = null
    try {
      builder = factory.newDocumentBuilder
      val out: dom.Document = builder.newDocument
      convert(in, out)
      out
    } catch {
      case e: ParserConfigurationException =>
        throw new IllegalStateException(e)
    }
  }

  /**
   * Converts a jsoup document into the provided W3C Document. If required, you can set options on the output document
   * before converting.
   * @param in jsoup doc
   * @param out w3c doc
   * @see org.jsoup.helper.W3CDom#fromJsoup(org.jsoup.nodes.Document)
   */
  def convert(in: nodes.Document, out: org.w3c.dom.Document) {
    if (!Strings.isBlank(in.location)) {
      out.setDocumentURI(in.location)
    }
    val rootEl: nodes.Element = in.child(0) // skip the #root node
    val traversor: NodeTraversor = new NodeTraversor(new W3CBuilder(out))
    traversor.traverse(rootEl)
  }

  /**
   * Implements the conversion by walking the input.
   */
  protected class W3CBuilder(doc: dom.Document) extends NodeVisitor {

    private var dest: dom.Element = null

    def head(source: nodes.Node, depth: Int) {
      source match {
        case sourceEl: nodes.Element =>
          val el: dom.Element = doc.createElement(sourceEl.tagName)
          copyAttributes(sourceEl, el)
          if (dest == null) {
            // sets up the root
            doc.appendChild(el)
          } else {
            dest.appendChild(el)
          }
          dest = el // descend
        case sourceText: nodes.TextNode =>
          val text: dom.Text = doc.createTextNode(sourceText.getWholeText)
          dest.appendChild(text)
        case sourceComment: nodes.Comment =>
          val comment: dom.Comment = doc.createComment(sourceComment.getData)
          dest.appendChild(comment)
        case sourceData: nodes.DataNode =>
          val node: dom.Text = doc.createTextNode(sourceData.getWholeData)
          dest.appendChild(node)
        case _ => // unhandled
      }
    }

    def tail(source: nodes.Node, depth: Int) {
      if (source.isInstanceOf[nodes.Element] && dest.getParentNode.isInstanceOf[dom.Element]) {
        dest = dest.getParentNode.asInstanceOf[dom.Element]
      }
    }

    private def copyAttributes(source: nodes.Node, el: dom.Element) {
      source.attributes.foreach(attribute => el.setAttribute(attribute.getKey, attribute.getValue))
    }
  }

  /**
   * Serialize a W3C document to a String.
   * @param doc Document
   * @return Document as string
   */
  def asString(doc: dom.Document): String = {
    try {
      val domSource: DOMSource = new DOMSource(doc)
      val writer: StringWriter = new StringWriter
      val result: StreamResult = new StreamResult(writer)
      val tf: TransformerFactory = TransformerFactory.newInstance
      val transformer: Transformer = tf.newTransformer
      transformer.transform(domSource, result)
      writer.toString
    } catch {
      case e: TransformerException =>
        throw new IllegalStateException(e)
    }
  }

}
