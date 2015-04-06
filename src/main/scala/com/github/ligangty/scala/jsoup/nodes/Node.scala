package com.github.ligangty.scala.jsoup.nodes

import java.net.{MalformedURLException, URL}

import com.github.ligangty.scala.jsoup.helper.Strings
import com.github.ligangty.scala.jsoup.select.{Elements, NodeTraversor, NodeVisitor}

import scala.collection.mutable

import com.github.ligangty.scala.jsoup.helper.Validator._

import scala.collection.mutable.ArrayBuffer

/**
 * The base, abstract Node model. Elements, Documents, Comments etc are all Node instances.
 */
abstract class Node private(u: Unit = ()) extends scala.Cloneable {

  private[nodes] var parentNodeVal: Node = null
  private[nodes] var childNodes: mutable.Buffer[Node] = null
  private[nodes] var attributesVal: Attributes = null
  private[nodes] var baseUriVal: String = null
  private[nodes] var siblingIndexVal: Int = 0

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
    this.attributesVal = attributes
  }

  protected def this(baseUri: String) {
    this(baseUri, new Attributes)
  }

  /**
   * Default constructor. Doesn't setup base uri, children, or attributes; use with caution.
   */
  protected def this() {
    this(())
    childNodes = mutable.Buffer.empty
    attributesVal = null
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
   * which is a shortcut to the [[absUrl]] method.
   * E.g.: <blockquote><code>String url = a.attr("abs:href");</code></blockquote>
   * @param attributeKey The attribute key.
   * @return The attribute, or empty string if not present (to avoid nulls).
   * @see #attributes()
   * @see #hasAttr(String)
   * @see #absUrl(String)
   */
  def attr(attributeKey: String): String = {
    notNull(attributeKey)
    if (attributesVal.hasKey(attributeKey)) {
      attributesVal.get(attributeKey)
    } else if (attributeKey.toLowerCase.startsWith("abs:")) {
      absUrl(attributeKey.substring("abs:".length))
    } else {
      ""
    }
  }

  /**
   * Get all of the element's attributes.
   * @return attributes (which implements iterable, in same order as presented in original HTML).
   */
  def attributes: Attributes = attributesVal

  /**
   * Set an attribute (key=value). If the attribute already exists, it is replaced.
   * @param attributeKey The attribute key.
   * @param attributeValue The attribute value.
   * @return this (for chaining)
   */
  def attr(attributeKey: String, attributeValue: String): Node = {
    attributesVal.put(attributeKey, attributeValue)
    this
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
      if (attributesVal.hasKey(key) && !(absUrl(key) == "")) {
        return true
      }
    }
    attributesVal.hasKey(attributeKey)
  }

  /**
   * Remove an attribute from this element.
   * @param attributeKey The attribute to remove.
   * @return this (for chaining)
   */
  def removeAttr(attributeKey: String): Node = {
    notNull(attributeKey)
    attributesVal.remove(attributeKey)
    this
  }

  /**
   * Get the base URI of this node.
   * @return base URI
   */
  def baseUri: String = baseUriVal

  /**
   * Update the base URI of this node and all of its descendants.
   * @param baseUri base URI to set
   */
  def setBaseUri(baseUri: String) {
    notNull(baseUri)
    traverse(new NodeVisitor {
      def head(node: Node, depth: Int) {
        node.baseUriVal = baseUri
      }

      def tail(node: Node, depth: Int) {}
    })
  }

  /**
   * Get an absolute URL from a URL attribute that may be relative (i.e. an <code>&lt;a href></code> or
   * <code>&lt;img src></code>).
   * <p/>
   * E.g.: <code>String absUrl = linkEl.absUrl("href");</code>
   * <p/>
   * If the attribute value is already absolute (i.e. it starts with a protocol, like
   * <code>http://</code> or <code>https://</code> etc), and it successfully parses as a URL, the attribute is
   * returned directly. Otherwise, it is treated as a URL relative to the element's [[baseUri]], and made
   * absolute using that.
   * <p/>
   * As an alternate, you can use the [[attr]] method with the <code>abs:</code> prefix, e.g.:
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
      ""
    } else {
      var base: URL = null
      try {
        try {
          base = new URL(baseUriVal)
        }
        catch {
          case e: MalformedURLException =>
            val abs: URL = new URL(relUrl)
            return abs.toExternalForm
        }
        if (relUrl.startsWith("?")) {
          relUrl = base.getPath + relUrl
        }
        val abs: URL = new URL(base, relUrl)
        abs.toExternalForm
      } catch {
        case e: MalformedURLException => ""
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
  def childNodesCopy: Seq[Node] = childNodes.map(_.clone()).toSeq

  /**
   * Get the number of child nodes that this node holds.
   * @return the number of child nodes that this node holds.
   */
  def childNodeSize: Int = childNodes.size

  protected def childNodesAsArray: Array[Node] = childNodes.toArray

  /**
   * Gets this node's parent node.
   * @return parent node; or null if no parent.
   */
  def parent: Node = parentNodeVal

  /**
   * Gets this node's parent node. Node overridable by extending classes, so useful if you really just need the Node type.
   * @return parent node; or null if no parent.
   */
  def parentNode: Node = parentNodeVal

  /**
   * Gets the Document associated with this Node.
   * @return the Document associated with this Node, or null if there is no such Document.
   */
  def ownerDocument: Document = {
    this match {
      case d: Document => return d
    }
    if (parentNodeVal == null) {
      null
    } else {
      parentNodeVal.ownerDocument
    }
  }

  /**
   * Remove (delete) this node from the DOM tree. If this node has children, they are also removed.
   */
  def remove(): Unit = {
    notNull(parentNodeVal)
    parentNodeVal.removeChild(this)
  }

  /**
   * Insert the specified HTML into the DOM before this node (i.e. as a preceding sibling).
   * @param html HTML to add before this node
   * @return this node, for chaining
   * @see #after(String)
   */
  def before(html: String): Node = {
    addSiblingHtml(siblingIndex, html)
    this
  }

  /**
   * Insert the specified node into the DOM before this node (i.e. as a preceding sibling).
   * @param node to add before this node
   * @return this node, for chaining
   * @see #after(Node)
   */
  def before(node: Node): Node = {
    notNull(node)
    notNull(parentNodeVal)
    parentNodeVal.addChildren(siblingIndex, node)
    this
  }

  /**
   * Insert the specified HTML into the DOM after this node (i.e. as a following sibling).
   * @param html HTML to add after this node
   * @return this node, for chaining
   * @see #before(String)
   */
  def after(html: String): Node = {
    addSiblingHtml(siblingIndex + 1, html)
    this
  }

  /**
   * Insert the specified node into the DOM after this node (i.e. as a following sibling).
   * @param node to add after this node
   * @return this node, for chaining
   * @see #before(Node)
   */
  def after(node: Node): Node = {
    notNull(node)
    notNull(parentNodeVal)
    parentNodeVal.addChildren(siblingIndex + 1, node)
    this
  }

  private def addSiblingHtml(index: Int, html: String) {
    //    notNull(html)
    //    notNull(parentNodeVal)
    //    val context: Element = if (parent.isInstanceOf[Element]) parent.asInstanceOf[Element] else null
    //    val nodes: List[Node] = Parser.parseFragment(html, context, baseUri)
    //    parentNodeVal.addChildren(index, nodes.toArray(new Array[Node](nodes.size)))
  }

  /**
   * Wrap the supplied HTML around this node.
   * @param html HTML to wrap around this element, e.g. { @code <div class="head"></div>}. Can be arbitrarily deep.
   * @return this node, for chaining.
   */
  def wrap(html: String): Node = {
    //    notEmpty(html)
    //    val context: Element = if (parent.isInstanceOf[Element]) parent.asInstanceOf[Element] else null
    //    val wrapChildren: List[Node] = Parser.parseFragment(html, context, baseUri)
    //    val wrapNode: Node = wrapChildren.get(0)
    //    if (wrapNode == null || !(wrapNode.isInstanceOf[Element])) return null
    //    val wrap: Element = wrapNode.asInstanceOf[Element]
    //    val deepest: Element = getDeepChild(wrap)
    //    parentNodeVal.replaceChild(this, wrap)
    //    deepest.addChildren(this)
    //    if (wrapChildren.size > 0) {
    //      {
    //        var i: Int = 0
    //        while (i < wrapChildren.size) {
    //          {
    //            val remainder: Node = wrapChildren.get(i)
    //            remainder.parentNodeVal.removeChild(remainder)
    //            wrap.appendChild(remainder)
    //          }
    //          ({
    //            i += 1; i - 1
    //          })
    //        }
    //      }
    //    }
    this
  }

  /**
   * Removes this node from the DOM, and moves its children up into the node's parent. This has the effect of dropping
   * the node but keeping its children.
   * <p/>
   * For example, with the input html:<br/>
   * {{{<div>One <span>Two <b>Three</b></span></div>}}}<br/>
   * Calling <code>element.unwrap()</code> on the <code>span</code> element will result in the html:<br/>
   * {{{<div>One Two <b>Three</b></div>}}}<br/>
   * and the <code>"Two "</code> [[TextNode]] being returned.
   * @return the first child of this node, after the node has been unwrapped. Null if the node had no children.
   * @see [[remove()]]
   * @see [[wrap(String)]]
   */
  def unwrap: Node = {
    notNull(parentNodeVal)
    val index: Int = siblingIndex
    val firstChild: Node = if (childNodes.size > 0) {
      childNodes(0)
    } else {
      null
    }
    parentNodeVal.addChildren(index, this.childNodesAsArray)
    this.remove()
    firstChild
  }

  private def getDeepChild(el: Element): Element = {
    val children: Elements = el.children
    if (children.size > 0) {
      getDeepChild(children.get(0))
    } else {
      el
    }
  }

  /**
   * Replace this node in the DOM with the supplied node.
   * @param in the node that will will replace the existing node.
   */
  def replaceWith(in: Node) {
    notNull(in)
    notNull(parentNodeVal)
    parentNodeVal.replaceChild(this, in)
  }

  protected def setParentNode(parentNode: Node) {
    if (this.parentNodeVal != null) {
      this.parentNodeVal.removeChild(this)
    }
    this.parentNodeVal = parentNode
  }

  protected def replaceChild(out: Node, in: Node) {
    isTrue(out.parentNodeVal eq this)
    notNull(in)
    if (in.parentNodeVal != null) {
      in.parentNodeVal.removeChild(in)
    }
    val index: Integer = out.siblingIndex
    childNodes(index) = in
    in.parentNodeVal = this
    in.setSiblingIndex(index)
    out.parentNodeVal = null
  }

  protected def removeChild(out: Node) {
    isTrue(out.parentNodeVal eq this)
    val index: Int = out.siblingIndex
    childNodes.remove(index)
    reindexChildren()
    out.parentNodeVal = null
  }

  protected def addChildren(children: Node*): Unit = addChildren(children.toArray)

  protected def addChildren(children: Array[Node]): Unit = {
    for (child <- children) {
      reparentChild(child)
      childNodes.append(child)
      child.setSiblingIndex(childNodes.size - 1)
    }
  }

  protected[nodes] def addChildren(index: Int, children: Node*): Unit = addChildren(index, children.toArray)

  protected[nodes] def addChildren(index: Int, children: Array[Node]): Unit = {
    noNullElements(children)
    var i: Int = children.length - 1
    while (i >= 0) {
      val in: Node = children(i)
      reparentChild(in)
      childNodes.insert(index, in)
      i -= 1
    }
    reindexChildren()
  }

  protected def reparentChild(child: Node) {
    if (child.parentNodeVal != null) {
      child.parentNodeVal.removeChild(child)
    }
    child.setParentNode(this)
  }

  private def reindexChildren(): Unit = {
    var i: Int = 0
    val length = childNodes.size
    while (i < length) {
      childNodes(i).setSiblingIndex(i)
      i += 1
    }
  }

  /**
   * Retrieves this node's sibling nodes. Similar to [[childNodes()]]  [[parent.childNodes()}]], but does not
   * include this node (a node is not a sibling of itself).
   * @return node siblings. If the node has no parent, returns an empty list.
   */
  def siblingNodes: List[Node] = {
    if (parentNodeVal == null) {
      return List.empty
    }
    val nodes: mutable.Buffer[Node] = parentNodeVal.childNodes
    val siblings: mutable.Buffer[Node] = new mutable.ArrayBuffer[Node](nodes.size - 1)
    import scala.collection.JavaConversions._
    for (node <- nodes) {
      if (node ne this) {
        siblings.add(node)
      }
    }
    siblings.toList
  }

  /**
  Get this node's next sibling.
     @return next sibling, or null if this is the last sibling
    */
  def nextSibling: Node = {
    if (parentNodeVal == null) {
      return null
    }
    val siblings: mutable.Buffer[Node] = parentNodeVal.childNodes
    val index: Integer = siblingIndex
    notNull(index)
    if (siblings.size > index + 1) {
      siblings(index + 1)
    } else {
      null
    }
  }

  /**
  Get this node's previous sibling.
     @return the previous sibling, or null if this is the first sibling
    */
  def previousSibling: Node = {
    if (parentNodeVal == null) {
      return null
    }
    val siblings: mutable.Buffer[Node] = parentNodeVal.childNodes
    val index: Integer = siblingIndex
    notNull(index)
    if (index > 0) {
      siblings(index - 1)
    } else {
      null
    }
  }

  /**
   * Get the list index of this node in its node sibling list. I.e. if this is the first node
   * sibling, returns 0.
   * @return position in node sibling list
   * @see org.jsoup.nodes.Element#elementSiblingIndex()
   */
  def siblingIndex: Int = siblingIndexVal

  protected[nodes] def setSiblingIndex(siblingIndex: Int) {
    this.siblingIndexVal = siblingIndex
  }

  /**
   * Perform a depth-first traversal through this node and its descendants.
   * @param nodeVisitor the visitor callbacks to perform on each node
   * @return this node, for chaining
   */
  def traverse(nodeVisitor: NodeVisitor): Node = {
    notNull(nodeVisitor)
    val traversor: NodeTraversor = new NodeTraversor(nodeVisitor)
    traversor.traverse(this)
    this
  }

  /**
   * Get the outer HTML of this node.
   * @return HTML
   */
  def outerHtml: String = {
    val accum: StringBuilder = new StringBuilder(128)
    outerHtml(accum)
    accum.toString()
  }

  protected[nodes] def outerHtml(accum: scala.StringBuilder) {
    new NodeTraversor(new Node.OuterHtmlVisitor(accum, getOutputSettings)).traverse(this)
  }

  // if this node has no document (or parent), retrieve the default output settings
  private[nodes] def getOutputSettings: Document.OutputSettings = {
    if (ownerDocument != null) {
      ownerDocument.outputSettings
    } else {
      new Document("").outputSettings
    }
  }

  /**
   * Get the outer HTML of this node.
   * @param accum accumulator to place HTML into
   */
  private[nodes] def outerHtmlHead(accum: StringBuilder, depth: Int, out: Document.OutputSettings)

  private[nodes] def outerHtmlTail(accum: StringBuilder, depth: Int, out: Document.OutputSettings)

  override def toString: String = {
    outerHtml
  }

  protected def indent(accum: StringBuilder, depth: Int, out: Document.OutputSettings) {
    accum.append("\n").append(Strings.padding(depth * out.indentAmount))
  }

  override def equals(o: Any): Boolean = o match {
    case node: Node => this eq node
    case _ => false
  }

  override def hashCode: Int = {
    var result: Int = if (parentNodeVal != null) {
      parentNodeVal.hashCode
    } else {
      0
    }
    result = 31 * result + (if (attributesVal != null) {
      attributesVal.hashCode
    } else {
      0
    })
    result
  }

  /**
   * Create a stand-alone, deep copy of this node, and all of its children. The cloned node will have no siblings or
   * parent node. As a stand-alone object, any changes made to the clone or any of its children will not impact the
   * original node.
   * <p>
   * The cloned node may be adopted into another Document or node structure using [[Element.appendChild(Node)]].
   * @return stand-alone cloned node
   */
  override def clone(): Node = {
    val thisClone: Node = doClone(null)
    val nodesToProcess: mutable.Queue[Node] = mutable.Queue[Node](thisClone)
    while (nodesToProcess.nonEmpty) {
      val currParent: Node = nodesToProcess.dequeue()
      var i: Int = 0
      val length = currParent.childNodes.size
      while (i < length) {
        val childClone: Node = currParent.childNodes(i).doClone(currParent)
        currParent.childNodes(i) = childClone
        nodesToProcess.enqueue(childClone)
        i += 1
      }
    }
    thisClone
  }

  protected def doClone(parent: Node): Node = {
    var clone: Node = null
    try {
      clone = super.clone.asInstanceOf[Node]
    }
    catch {
      case e: CloneNotSupportedException =>
        throw new RuntimeException(e)
    }
    clone.parentNodeVal = parent
    clone.siblingIndexVal = if (parent == null) {
      0
    } else {
      this.siblingIndexVal
    }
    clone.attributesVal = if (this.attributesVal != null) {
      attributesVal.clone
    } else {
      null
    }
    clone.baseUriVal = baseUri
    clone.childNodes = new mutable.ArrayBuffer[Node](childNodes.size)
    import scala.collection.JavaConversions._
    for (child <- this.childNodes) {
      clone.childNodes.add(child)
    }
    clone
  }

}

object Node {

  private[Node] class OuterHtmlVisitor extends NodeVisitor {

    private var accum: StringBuilder = null
    private var out: Document.OutputSettings = null

    private[nodes] def this(accum: StringBuilder, out: Document.OutputSettings) {
      this()
      this.accum = accum
      this.out = out
    }

    def head(node: Node, depth: Int) {
      node.outerHtmlHead(accum, depth, out)
    }

    def tail(node: Node, depth: Int) {
      if (!(node.nodeName == "#text")) {
        node.outerHtmlTail(accum, depth, out)
      }
    }
  }

}