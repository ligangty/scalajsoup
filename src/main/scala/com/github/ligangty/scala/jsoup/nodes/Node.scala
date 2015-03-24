package com.github.ligangty.scala.jsoup.nodes

import java.net.{MalformedURLException, URL}

import scala.collection.mutable

import com.github.ligangty.scala.jsoup.helper.Validator._

import scala.collection.mutable.ArrayBuffer


/**
 * Created by gli on 3/9/15.
 */
abstract class Node private(u: Unit = ()) extends scala.Cloneable {
  private[nodes] var parentNode: Node = null
  private[nodes] var childNodes: mutable.Seq[Node] = null
  private[nodes] var attributes: Attributes = null
  private[nodes] var baseUriVal: String = null
  private[nodes] var siblingIndex: Int = 0

  /**
   * Create a new Node.
   * @param baseUri base URI
   * @param attributes attributes (not null, but may be empty)
   */
  protected def this(baseUri: String, attributes: Attributes) {
    this(())
    notNull(baseUri)
    notNull(attributes)
    childNodes = new ArrayBuffer[Node]()
    this.baseUriVal = baseUri.trim
    this.attributes = attributes
  }

  protected def this(baseUri: String) {
    this(baseUri, new Attributes)
  }

  /**
   * Default constructor. Doesn't setup base uri, children, or attributes; use with caution.
   */
  protected def this() {
    this(())
    childNodes = mutable.Seq.empty
    attributes = null
  }

  /**
   * Get the node name of this node. Use for debugging purposes and not logic switching (for that, use instanceof).
   * @return node name
   */
  def nodeName(): String

  /**
   * Get an attribute's value by its key.
   * <p/>
   * To get an absolute URL from an attribute that may be a relative URL, prefix the key with <code><b>abs</b></code>,
   * which is a shortcut to the {@link #absUrl} method.
   * E.g.: <blockquote><code>String url = a.attr("abs:href");</code></blockquote>
   * @param attributeKey The attribute key.
   * @return The attribute, or empty string if not present (to avoid nulls).
   * @see #attributes()
   * @see #hasAttr(String)
   * @see #absUrl(String)
   */
  def attr(attributeKey: String): String = {
    notNull(attributeKey)
    if (attributes.hasKey(attributeKey)) attributes.get(attributeKey)
    else if (attributeKey.toLowerCase.startsWith("abs:")) absUrl(attributeKey.substring("abs:".length))
    else ""
  }

  /**
   * Test if this element has an attribute.
   * @param attributeKey The attribute key to check.
   * @return true if the attribute exists, false if not.
   */
  def hasAttr(attributeKey: String): Boolean = {
    notNull(attributeKey)
    if (attributeKey.startsWith("abs:")) {
      val key: String = attributeKey.substring("abs:".length)
      if (attributes.hasKey(key) && !(absUrl(key) == "")) return true
    }
    attributes.hasKey(attributeKey)
  }

  /**
   * Remove an attribute from this element.
   * @param attributeKey The attribute to remove.
   * @return this (for chaining)
   */
  def removeAttr(attributeKey: String): Node = {
    notNull(attributeKey)
    attributes.remove(attributeKey)
    this
  }

  /**
   * Get the base URI of this node.
   * @return base URI
   */
  def baseUri: String = baseUriVal

  /**
  Update the base URI of this node and all of its descendants.
     @param baseUri base URI to set
    */
  def setBaseUri(baseUri: String) {
    //    notNull(baseUri)
    //    traverse( new class null {
    //      def head(node: Node, depth: Int) {
    //        node.baseUri = baseUri
    //      }
    //      def tail(node: Node, depth: Int) {
    //      }
    //    })
  }

  /**
   * Get an absolute URL from a URL attribute that may be relative (i.e. an <code>&lt;a href></code> or
   * <code>&lt;img src></code>).
   * <p/>
   * E.g.: <code>String absUrl = linkEl.absUrl("href");</code>
   * <p/>
   * If the attribute value is already absolute (i.e. it starts with a protocol, like
   * <code>http://</code> or <code>https://</code> etc), and it successfully parses as a URL, the attribute is
   * returned directly. Otherwise, it is treated as a URL relative to the element's {@link #baseUri}, and made
   * absolute using that.
   * <p/>
   * As an alternate, you can use the {@link #attr} method with the <code>abs:</code> prefix, e.g.:
   * <code>String absUrl = linkEl.attr("abs:href");</code>
   *
   * @param attributeKey The attribute key
   * @return An absolute URL if one could be made, or an empty string (not null) if the attribute was missing or
   *         could not be made successfully into a URL.
   * @see #attr
   * @see java.net.URL#URL(java.net.URL, String)
   */
  def absUrl(attributeKey: String): String = {
    notEmpty(attributeKey)
    var relUrl: String = attr(attributeKey)
    if (!hasAttr(attributeKey)) {
      return ""
    } else {
      var base: URL = null
      try {
        try {
          base = new URL(baseUriVal)
        }
        catch {
          case e: MalformedURLException => {
            val abs: URL = new URL(relUrl)
            return abs.toExternalForm
          }
        }
        if (relUrl.startsWith("?")) relUrl = base.getPath + relUrl
        val abs: URL = new URL(base, relUrl)
        return abs.toExternalForm
      }
      catch {
        case e: MalformedURLException => {
          return ""
        }
      }
    }
  }

  /**
  Get a child node by its 0-based index.
     @param index index of child node
  @return the child node at this index. Throws a { @code IndexOutOfBoundsException} if the index is out of bounds.
    */
  def getChildNode(index: Int): Node = childNodes(index)


  /**
  Get this node's children. Presented as an unmodifiable list: new children can not be added, but the child nodes
     themselves can be manipulated.
     @return list of children. If no children, returns an empty list.
    */
  def getChildNodes: Seq[Node] = childNodes.toSeq

  /**
   * Returns a deep copy of this node's children. Changes made to these nodes will not be reflected in the original
   * nodes
   * @return a deep copy of this node's children
   */
  def childNodesCopy: Seq[Node] = childNodes.map(_.clone).toSeq

  override def equals(o: Any): Boolean = o match {
    case node:Node => this eq node
    case _ => false
  }

  override def hashCode: Int = {
    var result: Int = if (parentNode != null) parentNode.hashCode else 0
    result = 31 * result + (if (attributes != null) attributes.hashCode else 0)
    result
  }

  /**
   * Create a stand-alone, deep copy of this node, and all of its children. The cloned node will have no siblings or
   * parent node. As a stand-alone object, any changes made to the clone or any of its children will not impact the
   * original node.
   * <p>
   * The cloned node may be adopted into another Document or node structure using {@link Element#appendChild(Node)}.
   * @return stand-alone cloned node
   */
  override def clone(): Node = {
    val thisClone: Node = doClone(null)
//    val nodesToProcess: mutable.MutableList[Node] = mutable.MutableList[Node](thisClone)
//    while (!nodesToProcess.isEmpty) {
//      val currParent: Node = nodesToProcess.remove
//      {
//        var i: Int = 0
//        while (i < currParent.childNodes.size) {
//          {
//            val childClone: Node = currParent.childNodes.get(i).doClone(currParent)
//            currParent.childNodes.set(i, childClone)
//            nodesToProcess.add(childClone)
//          }
//          ({
//            i += 1; i - 1
//          })
//        }
//      }
//    }
    return thisClone
  }

  protected def doClone(parent: Node): Node = {
    var clone: Node = null
    try {
      clone = super.clone.asInstanceOf[Node]
    }
    catch {
      case e: CloneNotSupportedException => {
        throw new RuntimeException(e)
      }
    }
    clone.parentNode = parent
    clone.siblingIndex = if (parent == null) 0 else siblingIndex
    clone.attributes = if (attributes != null) attributes.clone else null
    clone.baseUriVal = baseUri
    clone.childNodes = new mutable.ArrayBuffer[Node](childNodes.size)
    import scala.collection.JavaConversions._
    for (child <- childNodes) clone.childNodes.add(child)
    return clone
  }

//  private class OuterHtmlVisitor extends NodeVisitor {
//    private var accum: StringBuilder = null
//    private var out: Document.OutputSettings = null
//
//    private[nodes] def this(accum: StringBuilder, out: Document.OutputSettings) {
//      this()
//      this.accum = accum
//      this.out = out
//    }
//
//    def head(node: Node, depth: Int) {
//      node.outerHtmlHead(accum, depth, out)
//    }
//
//    def tail(node: Node, depth: Int) {
//      if (!(node.nodeName == "#text")) node.outerHtmlTail(accum, depth, out)
//    }
//  }

}
