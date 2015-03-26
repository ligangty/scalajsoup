package com.github.ligangty.scala.jsoup.nodes

import java.util

import com.github.ligangty.scala.jsoup.helper.Validator
import com.github.ligangty.scala.jsoup.helper.Validator._
import com.github.ligangty.scala.jsoup.parser.Tag
import com.github.ligangty.scala.jsoup.select.Elements

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created by gli on 15-3-10.
 */
class Element(baseUri: String, attributes: Attributes) extends Node(baseUri, attributes) {
  private var tagVal: Tag = null

  /**
   * Create a new, standalone Element. (Standalone in that is has no parent.)
   *
   * @param tag tagVal of this element
   * @param baseUri the base URI
   * @param attributes initial attributes
   * @see #appendChild(Node)
   * @see #appendElement(String)
   */
  def this(tag: Tag, baseUri: String, attributes: Attributes) {
    this(baseUri, attributes)
    notNull(tag)
    this.tagVal = tag
  }

  /**
   * Create a new Element from a tagVal and a base URI.
   *
   * @param tag element tagVal
   * @param baseUri the base URI of this element. It is acceptable for the base URI to be an empty
   *                string, but not null.
   * @see Tag#valueOf(String)
   */
  def this(tag: Tag, baseUri: String) {
    this(tag, baseUri, new Attributes)
  }

  def nodeName: String = tagVal.getName

  /**
   * Get the name of the tagVal for this element. E.g. {@code div}
   *
   * @return the tagVal name
   */
  def tagName: String = tagVal.getName

  /**
   * Change the tag of this element. For example, convert a {@code <span>} to a {@code <div>} with
   * {@code el.tagName("div");}.
   *
   * @param tagName new tag name for this element
   * @return this element, for chaining
   */
  def tagName(tagName: String): Element = {
    notEmpty(tagName, "Tag name must not be empty.")
    tagVal = Tag(tagName)
    this
  }

  /**
   * Get the Tag for this element.
   *
   * @return the tag object
   */
  def tag: Tag = tagVal

  /**
   * Test if this element is a block-level element. (E.g. {@code <div> == true} or an inline element
   * {@code <p> == false}).
   *
   * @return true if block, false if not (and thus inline)
   */
  def isBlock: Boolean = tag.isBlock


  /**
   * Get the {@code id} attribute of this element.
   *
   * @return The id attribute, if present, or an empty string if not.
   */
  def id: String = {
    val id: String = attr("id")
    if (id == null) "" else id
  }

  /**
   * Set an attribute value on this element. If this element already has an attribute with the
   * key, its value is updated; otherwise, a new attribute is added.
   *
   * @return this element
   */
  override def attr(attributeKey: String, attributeValue: String): Element = {
    super.attr(attributeKey, attributeValue)
    this
  }

  /**
   * Get this element's HTML5 custom data attributes. Each attribute in the element that has a key
   * starting with "data-" is included the dataset.
   * <p>
   * E.g., the element {@code <div data-package="jsoup" data-language="Java" class="group">...} has the dataset
   * {@code package=jsoup, language=java}.
   * <p>
   * This map is a filtered view of the element's attribute map. Changes to one map (add, remove, update) are reflected
   * in the other map.
   * <p>
   * You can find elements that have data attributes using the {@code [^data-]} attribute key prefix selector.
   * @return a map of { @code key=value} custom data attributes.
   */
  def dataset: Map[String, String] = attributes.dataset

  override def parent: Element = parentNode.asInstanceOf[Element]

  /**
   * Get this element's parent and ancestors, up to the document root.
   * @return this element's stack of parents, closest first.
   */
  def parents: Elements = {
    val parents: Elements = new Elements
    accumulateParents(this, parents)
    parents
  }

  private def accumulateParents(el: Element, parents: Elements) {
    val parent: Element = el.parent
    if (parent != null && !(parent.tagName == "#root")) {
      parents.add(parent)
      accumulateParents(parent, parents)
    }
  }

  /**
   * Get a child element of this element, by its 0-based index number.
   * <p/>
   * Note that an element can have both mixed Nodes and Elements as children. This method inspects
   * a filtered list of children that are elements, and the index is based on that filtered list.
   *
   * @param index the index number of the element to retrieve
   * @return the child element, if it exists, otherwise throws an { @code IndexOutOfBoundsException}
   * @see #childNode(int)
   */
  def child(index: Int): Element = children.get(index)

  /**
   * Get this element's child elements.
   * <p/>
   * This is effectively a filter on {@link #childNodes()} to get Element nodes.
   * @return child elements. If this element has no children, returns an
   *         empty list.
   * @see #childNodes()
   */
  def children: Elements =
    new Elements(childNodes.filter(_.isInstanceOf[Element]).map(_.asInstanceOf[Element]))

  /**
   * Get this element's child text nodes. The list is unmodifiable but the text nodes may be manipulated.
   * <p/>
   * This is effectively a filter on {@link #childNodes()} to get Text nodes.
   * @return child text nodes. If this element has no text nodes, returns an
   *         empty list.
   *         <p/>
   *         For example, with the input HTML: { @code <p>One <span>Two</span> Three <br> Four</p>} with the { @code p} element selected:
   *         <ul>
   *         <li>{ @code p.text()} = { @code "One Two Three Four"}</li>
   *         <li>{ @code p.ownText()} = { @code "One Three Four"}</li>
   *         <li>{ @code p.children()} = { @code Elements[<span>, <br>]}</li>
   *         <li>{ @code p.childNodes()} = { @code List<Node>["One ", <span>, " Three ", <br>, " Four"]}</li>
   *         <li>{ @code p.textNodes()} = { @code List<TextNode>["One ", " Three ", " Four"]}</li>
   *         </ul>
   */
  def textNodes: List[TextNode] =
    childNodes.filter(_.isInstanceOf[TextNode]).map(_.asInstanceOf[TextNode]).toList


  /**
   * Get this element's child data nodes. The list is unmodifiable but the data nodes may be manipulated.
   * <p/>
   * This is effectively a filter on {@link #childNodes()} to get Data nodes.
   * @return child data nodes. If this element has no data nodes, returns an
   *         empty list.
   * @see #data()
   */
  def dataNodes: List[DataNode] =
    childNodes.filter(_.isInstanceOf[DataNode]).map(_.asInstanceOf[DataNode]).toList

  /**
   * Find elements that match the {@link Selector} CSS query, with this element as the starting context. Matched elements
   * may include this element, or any of its children.
   * <p/>
   * This method is generally more powerful to use than the DOM-type {@code getElementBy*} methods, because
   * multiple filters can be combined, e.g.:
   * <ul>
   * <li>{@code el.select("a[href]")} - finds links ({@code a} tags with {@code href} attributes)
   * <li>{@code el.select("a[href*=example.com]")} - finds links pointing to example.com (loosely)
   * </ul>
   * <p/>
   * See the query syntax documentation in {@link org.jsoup.select.Selector}.
   *
   * @param cssQuery a { @link Selector} CSS-like query
   * @return elements that match the query (empty if none match)
   * @see org.jsoup.select.Selector
   */
  def select(cssQuery: String): Elements = {
    //TODO: need to implement Selector
//    return Selector.select(cssQuery, this)
    new Elements()
  }

  /**
   * Add a node child node to this element.
   *
   * @param child node to add.
   * @return this element, so that you can add more child nodes or elements.
   */
  def appendChild(child: Node): Element = {
    notNull(child)
    // was - Node#addChildren(child). short-circuits an array create and a loop.
    reparentChild(child)
    childNodes.append(child)
    child.setSiblingIndex(childNodes.size - 1)
    this
  }

  /**
   * Add a node to the start of this element's children.
   *
   * @param child node to add.
   * @return this element, so that you can add more child nodes or elements.
   */
  def prependChild(child: Node): Element = {
    notNull(child)
    addChildren(0, child)
    this
  }

  /**
   * Inserts the given child nodes into this element at the specified index. Current nodes will be shifted to the
   * right. The inserted nodes will be moved from their current parent. To prevent moving, copy the nodes first.
   *
   * @param index 0-based index to insert children at. Specify { @code 0} to insert at the start, { @code -1} at the
   *                                                                                                      end
   * @param children child nodes to insert
   * @return this element, for chaining.
   */
  def insertChildren(index: Int, children: util.Collection[_ <: Node]): Element = {
    Validator.notNull(children, "Children collection to be inserted must not be null.")
    var indexVal = index
    val currentSize: Int = childNodeSize
    if (indexVal < 0) indexVal += (currentSize + 1)
    isTrue(indexVal >= 0 && indexVal <= currentSize, "Insert position out of bounds.")
    val nodes: mutable.ArrayBuffer[Node] = new mutable.ArrayBuffer[Node](children)
    val nodeArray: Array[Node] = nodes.toArray(new Array[Node](nodes.size))
    addChildren(indexVal, nodeArray)
    return this
  }

  /**
   * Create a new element by tag name, and add it as the last child.
   *
   * @param tagName the name of the tag (e.g. { @code div}).
   * @return the new element, to allow you to add content to it, e.g.:
   *         { @code parent.appendElement("h1").attr("id", "header").text("Welcome");}
   */
  def appendElement(tagName: String): Element = {
    val child: Element = new Element(Tag.valueOf(tagName), baseUri)
    appendChild(child)
    return child
  }

  /**
   * Create a new element by tag name, and add it as the first child.
   *
   * @param tagName the name of the tag (e.g. { @code div}).
   * @return the new element, to allow you to add content to it, e.g.:
   *         { @code parent.prependElement("h1").attr("id", "header").text("Welcome");}
   */
  def prependElement(tagName: String): Element = {
    val child: Element = new Element(Tag.valueOf(tagName), baseUri)
    prependChild(child)
    return child
  }

  private[nodes] def outerHtmlHead(accum: StringBuilder, depth: Int, out: Document.OutputSettings) {
    if (accum.length > 0 &&
      out.prettyPrint &&
      (tagVal.isFormatAsBlock || (parent != null && parent.tagVal.isFormatAsBlock) || out.outline))
      indent(accum, depth, out)
    accum.append("<").append(tagName)
    attributes.html(accum, out)
    if (childNodes.isEmpty && tagVal.isSelfClosing) {
      if ((out.syntax eq Document.OutputSettings.Syntax.html) && tagVal.isEmpty) accum.append('>')
      else accum.append(" />")
    }
    else accum.append(">")
  }

  private[nodes] def outerHtmlTail(accum: StringBuilder, depth: Int, out: Document.OutputSettings) {
    if (!(childNodes.isEmpty && tagVal.isSelfClosing)) {
      if (out.prettyPrint && (!childNodes.isEmpty && (tagVal.isFormatAsBlock || (out.outline && (childNodes.size > 1 || (childNodes.size == 1 && !(childNodes(0).isInstanceOf[TextNode]))))))) indent(accum, depth, out)
      accum.append("</").append(tagName).append(">")
    }
  }


  /**
   * Retrieves the element's inner HTML. E.g. on a {@code <div>} with one empty {@code <p>}, would return
   * {@code <p></p>}. (Whereas {@link #outerHtml()} would return {@code <div><p></p></div>}.)
   *
   * @return String of HTML.
   * @see #outerHtml()
   */
  def html: String = {
    val accum: StringBuilder = new StringBuilder
    html(accum)
    return if (getOutputSettings.prettyPrint) accum.toString.trim else accum.toString
  }

  private def html(accum: StringBuilder) {
    for (node <- childNodes) node.outerHtml(accum)
  }

  /**
   * Set this element's inner HTML. Clears the existing HTML first.
   * @param html HTML to parse and set into this element
   * @return this element
   * @see #append(String)
   */
  def html(html: String): Element = {
    empty
    append(html)
    return this
  }

  override def toString: String = {
    return outerHtml
  }

  override def equals(o: AnyRef): Boolean = {
    return this eq o
  }

  override def hashCode: Int = {
    var result: Int = super.hashCode
    result = 31 * result + (if (tag != null) tag.hashCode else 0)
    return result
  }

  override def clone: Element = {
    val clone: Element = super.clone.asInstanceOf[Element]
    return clone
  }
}

private[nodes] object Element {
  private[nodes] def preserveWhitespace(node: Node): Boolean = {
    if (node != null && node.isInstanceOf[Element]) {
      val element: Element = node.asInstanceOf[Element]
      return element.tagVal.isPreserveWhitespace || element.parent != null && element.parent.tagVal.isPreserveWhitespace
    }
    false
  }
}
