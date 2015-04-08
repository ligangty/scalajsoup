package com.github.ligangty.scala.jsoup.nodes

import java.util

import com.github.ligangty.scala.jsoup.helper.Strings
import com.github.ligangty.scala.jsoup.helper.Validator._
import com.github.ligangty.scala.jsoup.parser.Tag
import com.github.ligangty.scala.jsoup.select.{NodeTraversor, NodeVisitor, Elements}
import Element._

import scala.collection.mutable

/**
 * A HTML element consists of a tag name, attributes, and child nodes (including text nodes and
 * other elements).
 *
 * From an Element, you can extract data, traverse the node graph, and manipulate the HTML.
 *
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

  override def nodeName(): String = tagVal.getName

  /**
   * Get the name of the tagVal for this element. E.g. <code>div</code>
   *
   * @return the tagVal name
   */
  def tagName: String = tagVal.getName

  /**
   * Change the tag of this element. For example, convert a <code><span></code> to a <code><div></code> with
   * <code>el.tagName("div");</code.
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
   * Test if this element is a block-level element. (E.g. {{{<div> == true}}} or an inline element
   * {{{<p> == false}}}).
   *
   * @return true if block, false if not (and thus inline)
   */
  def isBlock: Boolean = tag.isBlock

  /**
   * Get the <code>id</code> attribute of this element.
   *
   * @return The id attribute, if present, or an empty string if not.
   */
  def id: String = {
    val id: String = attr("id")
    if (id == null) {
      ""
    } else {
      id
    }
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
   * E.g., the element {{{<div data-package="jsoup" data-language="Java" class="group">...}}} has the dataset
   * {{{package=jsoup, language=java}}}.
   * <p>
   * This map is a filtered view of the element's attribute map. Changes to one map (add, remove, update) are reflected
   * in the other map.
   * <p>
   * You can find elements that have data attributes using the {{{[^data-]}}} attribute key prefix selector.
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
   * @return the child element, if it exists, otherwise throws an <code>IndexOutOfBoundsException</code
   * @see #childNode(int)
   */
  def child(index: Int): Element = children.get(index)

  /**
   * Get this element's child elements.
   * <p/>
   * This is effectively a filter on [[getChildNodes()]] to get Element nodes.
   * @return child elements. If this element has no children, returns an
   *         empty list.
   * @see #childNodes()
   */
  def children: Elements =
    new Elements(childNodes.filter(_.isInstanceOf[Element]).map(_.asInstanceOf[Element]))

  /**
   * Get this element's child text nodes. The list is unmodifiable but the text nodes may be manipulated.
   * <p/>
   * This is effectively a filter on [[getChildNodes()]] to get Text nodes.
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
   * This is effectively a filter on [[getChildNodes()]] to get Data nodes.
   * @return child data nodes. If this element has no data nodes, returns an
   *         empty list.
   * @see #data()
   */
  def dataNodes: List[DataNode] =
    childNodes.filter(_.isInstanceOf[DataNode]).map(_.asInstanceOf[DataNode]).toList

  /**
   * Find elements that match the [[com.github.ligangty.scala.jsoup.select.Selector]] CSS query, with this element as the starting context. Matched elements
   * may include this element, or any of its children.
   * <p/>
   * This method is generally more powerful to use than the DOM-type <code>getElementBy*</code methods, because
   * multiple filters can be combined, e.g.:
   * <ul>
   * <li><code>el.select("a[href]")</code> - finds links (<code>a</code> tags with <code>href</code> attributes)
   * <li><code>el.select("a[href*=example.com]")</code> - finds links pointing to example.com (loosely)
   * </ul>
   * <p/>
   * See the query syntax documentation in [[com.github.ligangty.scala.jsoup.select.Selector]].
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
   *              end
   * @param children child nodes to insert
   * @return this element, for chaining.
   */
  def insertChildren(index: Int, children: Seq[_ <: Node]): Element = {
    notNull(children, "Children collection to be inserted must not be null.")
    var indexVal = index
    val currentSize: Int = childNodeSize
    if (indexVal < 0) {
      indexVal += (currentSize + 1)
    }
    isTrue(indexVal >= 0 && indexVal <= currentSize, "Insert position out of bounds.")
    val nodes: mutable.ArrayBuffer[Node] = mutable.ArrayBuffer(children: _*)
    val nodeArray: Array[Node] = nodes.toArray
    addChildren(indexVal, nodeArray)
    this
  }

  /**
   * Create a new element by tag name, and add it as the last child.
   *
   * @param tagName the name of the tag (e.g. { @code div}).
   * @return the new element, to allow you to add content to it, e.g.:
   *         { @code parent.appendElement("h1").attr("id", "header").text("Welcome");}
   */
  def appendElement(tagName: String): Element = {
    val child: Element = new Element(Tag(tagName), baseUri)
    appendChild(child)
    child
  }

  /**
   * Create a new element by tag name, and add it as the first child.
   *
   * @param tagName the name of the tag (e.g. { @code div}).
   * @return the new element, to allow you to add content to it, e.g.:
   *         { @code parent.prependElement("h1").attr("id", "header").text("Welcome");}
   */
  def prependElement(tagName: String): Element = {
    val child: Element = new Element(Tag(tagName), baseUri)
    prependChild(child)
    child
  }

  /**
   * Create and append a new TextNode to this element.
   *
   * @param text the unencoded text to add
   * @return this element
   */
  def appendText(text: String): Element = {
    val node: TextNode = new TextNode(text, baseUri)
    appendChild(node)
    this
  }

  /**
   * Create and prepend a new TextNode to this element.
   *
   * @param text the unencoded text to add
   * @return this element
   */
  def prependText(text: String): Element = {
    val node: TextNode = new TextNode(text, baseUri)
    prependChild(node)
    this
  }

  /**
   * Add inner HTML to this element. The supplied HTML will be parsed, and each node appended to the end of the children.
   * @param html HTML to add inside this element, after the existing HTML
   * @return this element
   * @see #html(String)
   */
  def append(html: String): Element = {
    //TODO: Parser Not implemented yet
    //    notNull(html)
    //    val nodes: List[Node] = Parser.parseFragment(html, this, baseUri)
    //    addChildren(nodes.toArray)
    this
  }

  /**
   * Add inner HTML into this element. The supplied HTML will be parsed, and each node prepended to the start of the element's children.
   * @param html HTML to add inside this element, before the existing HTML
   * @return this element
   * @see #html(String)
   */
  def prepend(html: String): Element = {
    //TODO: Parser Not implemented yet
    //    notNull(html)
    //    val nodes: List[Node] = Parser.parseFragment(html, this, baseUri)
    //    addChildren(0, nodes.toArray)
    this
  }

  /**
   * Insert the specified HTML into the DOM before this element (as a preceding sibling).
   *
   * @param html HTML to add before this element
   * @return this element, for chaining
   * @see #after(String)
   */
  override def before(html: String): Element = super.before(html).asInstanceOf[Element]

  /**
   * Insert the specified node into the DOM before this node (as a preceding sibling).
   * @param node to add before this element
   * @return this Element, for chaining
   * @see #after(Node)
   */
  override def before(node: Node): Element = super.before(node).asInstanceOf[Element]

  /**
   * Insert the specified HTML into the DOM after this element (as a following sibling).
   *
   * @param html HTML to add after this element
   * @return this element, for chaining
   * @see #before(String)
   */
  override def after(html: String): Element = super.after(html).asInstanceOf[Element]

  /**
   * Insert the specified node into the DOM after this node (as a following sibling).
   * @param node to add after this element
   * @return this element, for chaining
   * @see #before(Node)
   */
  override def after(node: Node): Element = super.after(node).asInstanceOf[Element]

  /**
   * Remove all of the element's child nodes. Any attributes are left as-is.
   * @return this element
   */
  def empty: Element = {
    childNodes.clear()
    this
  }

  /**
   * Wrap the supplied HTML around this element.
   *
   * @param html HTML to wrap around this element, e.g. { @code <div class="head"></div>}. Can be arbitrarily deep.
   * @return this element, for chaining.
   */
  override def wrap(html: String): Element = super.wrap(html).asInstanceOf[Element]

  /**
   * Get a CSS selector that will uniquely select this element.
   * <p/>If the element has an ID, returns #id;
   * otherwise returns the parent (if any) CSS selector, followed by '>',
   * followed by a unique selector for the element (tag.class.class:nth-child(n)).
   *
   * @return the CSS Path that can be used to retrieve the element in a selector.
   */
  def cssSelector: String = {
    if (id.length > 0) {
      return "#" + id
    }
    val selector: StringBuilder = new StringBuilder(tagName)
    val classes: String = Strings.join(classNames, ".")
    if (classes.length > 0) {
      selector.append('.').append(classes)
    }
    if (parent == null || parent.isInstanceOf[Document]) {
      return selector.toString()
    }
    selector.insert(0, " > ")
    val value = ":nth-child(%d)".format(elementSiblingIndex + 1)
    if (parent.select(selector.toString()).size > 1) {
      selector.append(value)
    }
    parent.cssSelector + selector.toString()
  }

  /**
   * Get sibling elements. If the element has no sibling elements, returns an empty list. An element is not a sibling
   * of itself, so will not be included in the returned list.
   * @return sibling elements
   */
  def siblingElements: Elements = {
    if (parentNode == null) {
      return new Elements(0)
    }
    val elements: Elements = parent.children
    val siblings: Elements = new Elements(elements.size - 1)
    elements.foreach(a => if (a ne this) {
      siblings.add(a)
    })
    siblings
  }

  /**
   * Gets the next sibling element of this element. E.g., if a <code>div</code> contains two <code>p</code>s,
   * the <code>nextElementSibling</code> of the first <code>p</code> is the second <code>p</code>.
   * <p/>
   * This is similar to [[nextSibling()]], but specifically finds only Elements
   * @return the next element, or null if there is no next element
   * @see #previousElementSibling()
   */
  def nextElementSibling: Element = {
    if (parentNode == null) {
      return null
    }
    val siblings: Elements = parent.children
    val index: Integer = indexInList(this, siblings)
    notNull(index)
    if (siblings.size > index + 1) {
      siblings.get(index + 1)
    }
    else {
      null
    }
  }

  /**
   * Gets the previous element sibling of this element.
   * @return the previous element, or null if there is no previous element
   * @see #nextElementSibling()
   */
  def previousElementSibling: Element = {
    if (parentNode == null) {
      return null
    }
    val siblings: Elements = parent.children
    val index: Integer = indexInList(this, siblings)
    notNull(index)
    if (index > 0) {
      siblings.get(index - 1)
    }
    else {
      null
    }
  }

  /**
   * Gets the first element sibling of this element.
   * @return the first sibling that is an element (aka the parent's first element child)
   */
  def firstElementSibling: Element = {
    val siblings: Elements = parent.children
    if (siblings.size > 1) {
      siblings.get(0)
    } else {
      null
    }
  }

  /**
   * Get the list index of this element in its element sibling list. I.e. if this is the first element
   * sibling, returns 0.
   * @return position in element sibling list
   */
  def elementSiblingIndex: Int = {
    if (parent == null) {
      return 0
    }
    indexInList(this, parent.children)
  }

  /**
   * Gets the last element sibling of this element
   * @return the last sibling that is an element (aka the parent's last element child)
   */
  def lastElementSibling: Element = {
    val siblings: Elements = parent.children
    if (siblings.size > 1) {
      siblings.get(siblings.size - 1)
    } else {
      null
    }
  }

  private def indexInList[E <: Element](search: Element, elements: mutable.Seq[E]): Int = {
    notNull(search)
    notNull(elements)
    var i: Int = 0
    val length = elements.size
    while (i < length) {
      val element: E = elements(i)
      if (element == search) {
        return i
      }
      i += 1
    }
    //@todo make sure here should return 0
    0
  }

  // DOM type methods
  /**
   * Finds elements, including and recursively under this element, with the specified tag name.
   * @param tagName The tag name to search for (case insensitively).
   * @return a matching unmodifiable list of elements. Will be empty if this element and none of its children match.
   */
  def getElementsByTag(tagName: String): Elements = {
    //todo Collector and Evaluator not implemented yet
    //    notEmpty(tagName)
    //    tagName = tagName.toLowerCase.trim
    //    return Collector.collect(new Evaluator.Tag(tagName), this)
    null
  }

  /**
   * Find an element by ID, including or under this element.
   * <p>
   * Note that this finds the first matching ID, starting with this element. If you search down from a different
   * starting point, it is possible to find a different element by ID. For unique element by ID within a Document,
   * use [[Document#getElementById(String)]]
   * @param id The ID to search for.
   * @return The first matching element by ID, starting with this element, or null if none found.
   */
  def getElementById(id: String): Element = {
    //todo Collector and Evaluator not implemented yet
    //    notEmpty(id)
    //    val elements: Elements = Collector.collect(new Evaluator.Id(id), this)
    //    if (elements.size > 0) elements.get(0) else null
    null
  }

  /**
   * Find elements that have this class, including or under this element. Case insensitive.
   * <p>
   * Elements can have multiple classes (e.g. {{{<div class="header round first">}}}. This method
   * checks each class, so you can find the above with <code>el.getElementsByClass("header");</code.
   *
   * @param className the name of the class to search for.
   * @return elements with the supplied class name, empty if none
   * @see #hasClass(String)
   * @see #classNames()
   */
  def getElementsByClass(className: String): Elements = {
    //todo Collector and Evaluator not implemented yet
    //    notEmpty(className)
    //    Collector.collect(new Evaluator.Class(className), this)
    null
  }

  /**
   * Find elements that have a named attribute set. Case insensitive.
   *
   * @param key name of the attribute, e.g. { @code href}
   * @return elements that have this attribute, empty if none
   */
  def getElementsByAttribute(key: String): Elements = {
    //todo Collector and Evaluator not implemented yet
    notEmpty(key)
    //    Collector.collect(new Evaluator.Attribute(key.trim.toLowerCase), this)
    null
  }

  /**
   * Find elements that have an attribute name starting with the supplied prefix. Use <code>data-</code> to find elements
   * that have HTML5 datasets.
   * @param keyPrefix name prefix of the attribute e.g. { @code data-}
   * @return elements that have attribute names that start with with the prefix, empty if none.
   */
  def getElementsByAttributeStarting(keyPrefix: String): Elements = {
    //todo Collector and Evaluator not implemented yet
    //    notEmpty(keyPrefix)
    //    Collector.collect(new Evaluator.AttributeStarting(keyPrefix.trim.toLowerCase), this)
    null
  }

  /**
   * Find elements that have an attribute with the specific value. Case insensitive.
   *
   * @param key name of the attribute
   * @param value value of the attribute
   * @return elements that have this attribute with this value, empty if none
   */
  def getElementsByAttributeValue(key: String, value: String): Elements = {
    //todo Collector and Evaluator not implemented yet
    //    Collector.collect(new Evaluator.AttributeWithValue(key, value), this)
    null
  }

  /**
   * Find elements that either do not have this attribute, or have it with a different value. Case insensitive.
   *
   * @param key name of the attribute
   * @param value value of the attribute
   * @return elements that do not have a matching attribute
   */
  def getElementsByAttributeValueNot(key: String, value: String): Elements = {
    //todo Collector and Evaluator not implemented yet
    //    Collector.collect(new Evaluator.AttributeWithValueNot(key, value), this)
    null
  }

  /**
   * Find elements that have attributes that start with the value prefix. Case insensitive.
   *
   * @param key name of the attribute
   * @param valuePrefix start of attribute value
   * @return elements that have attributes that start with the value prefix
   */
  def getElementsByAttributeValueStarting(key: String, valuePrefix: String): Elements = {
    //todo Collector and Evaluator not implemented yet
    //    Collector.collect(new Evaluator.AttributeWithValueStarting(key, valuePrefix), this)
    null
  }

  /**
   * Find elements that have attributes that end with the value suffix. Case insensitive.
   *
   * @param key name of the attribute
   * @param valueSuffix end of the attribute value
   * @return elements that have attributes that end with the value suffix
   */
  def getElementsByAttributeValueEnding(key: String, valueSuffix: String): Elements = {
    //todo Collector and Evaluator not implemented yet
    //    Collector.collect(new Evaluator.AttributeWithValueEnding(key, valueSuffix), this)
    null
  }

  /**
   * Find elements that have attributes whose value contains the match string. Case insensitive.
   *
   * @param key name of the attribute
   * @param match substring of value to search for
   * @return elements that have attributes containing this text
   */
  def getElementsByAttributeValueContaining(key: String, `match`: String): Elements = {
    //todo Collector and Evaluator not implemented yet
    //    Collector.collect(new Evaluator.AttributeWithValueContaining(key, `match`), this)
    null
  }

  /**
   * Find elements that have attributes whose values match the supplied regular expression.
   * @param key name of the attribute
   * @param pattern compiled regular expression to match against attribute values
   * @return elements that have attributes matching this regular expression
   */
  def getElementsByAttributeValueMatching(key: String, pattern: util.regex.Pattern): Elements = {
    //todo Collector and Evaluator not implemented yet
    //    Collector.collect(new Evaluator.AttributeWithValueMatching(key, pattern), this)
    null
  }

  /**
   * Find elements that have attributes whose values match the supplied regular expression.
   * @param key name of the attribute
   * @param regex regular expression to match against attribute values. You can use <a href="http://java.sun.com/docs/books/tutorial/essential/regex/pattern.html#embedded">embedded flags</a> (such as (?i) and (?m) to control regex options.
   * @return elements that have attributes matching this regular expression
   */
  def getElementsByAttributeValueMatching(key: String, regex: String): Elements = {
    var pattern: util.regex.Pattern = null
    try {
      pattern = util.regex.Pattern.compile(regex)
    }
    catch {
      case e: util.regex.PatternSyntaxException =>
        throw new IllegalArgumentException("Pattern syntax error: " + regex, e)
    }
    getElementsByAttributeValueMatching(key, pattern)
  }

  /**
   * Find elements whose sibling index is less than the supplied index.
   * @param index 0-based index
   * @return elements less than index
   */
  def getElementsByIndexLessThan(index: Int): Elements = {
    //todo Collector and Evaluator not implemented yet
    //    return Collector.collect(new Evaluator.IndexLessThan(index), this)
    null
  }

  /**
   * Find elements whose sibling index is greater than the supplied index.
   * @param index 0-based index
   * @return elements greater than index
   */
  def getElementsByIndexGreaterThan(index: Int): Elements = {
    //todo Collector and Evaluator not implemented yet
    //    return Collector.collect(new Evaluator.IndexGreaterThan(index), this)
    null
  }

  /**
   * Find elements whose sibling index is equal to the supplied index.
   * @param index 0-based index
   * @return elements equal to index
   */
  def getElementsByIndexEquals(index: Int): Elements = {
    //todo Collector and Evaluator not implemented yet
    //    return Collector.collect(new Evaluator.IndexEquals(index), this)
    null
  }

  /**
   * Find elements that contain the specified string. The search is case insensitive. The text may appear directly
   * in the element, or in any of its descendants.
   * @param searchText to look for in the element's text
   * @return elements that contain the string, case insensitive.
   * @see Element#text()
   */
  def getElementsContainingText(searchText: String): Elements = {
    //todo Collector and Evaluator not implemented yet
    //    return Collector.collect(new Evaluator.ContainsText(searchText), this)
    null
  }

  /**
   * Find elements that directly contain the specified string. The search is case insensitive. The text must appear directly
   * in the element, not in any of its descendants.
   * @param searchText to look for in the element's own text
   * @return elements that contain the string, case insensitive.
   * @see Element#ownText()
   */
  def getElementsContainingOwnText(searchText: String): Elements = {
    //todo Collector and Evaluator not implemented yet
    //    return Collector.collect(new Evaluator.ContainsOwnText(searchText), this)
    null
  }

  /**
   * Find elements whose text matches the supplied regular expression.
   * @param pattern regular expression to match text against
   * @return elements matching the supplied regular expression.
   * @see Element#text()
   */
  def getElementsMatchingText(pattern: util.regex.Pattern): Elements = {
    //todo Collector and Evaluator not implemented yet
    //    return Collector.collect(new Evaluator.Matches(pattern), this)
    null
  }

  /**
   * Find elements whose text matches the supplied regular expression.
   * @param regex regular expression to match text against. You can use <a href="http://java.sun.com/docs/books/tutorial/essential/regex/pattern.html#embedded">embedded flags</a> (such as (?i) and (?m) to control regex options.
   * @return elements matching the supplied regular expression.
   * @see Element#text()
   */
  def getElementsMatchingText(regex: String): Elements = {
    var pattern: util.regex.Pattern = null
    try {
      pattern = util.regex.Pattern.compile(regex)
    }
    catch {
      case e: util.regex.PatternSyntaxException =>
        throw new IllegalArgumentException("Pattern syntax error: " + regex, e)
    }
    getElementsMatchingText(pattern)
  }

  /**
   * Find elements whose own text matches the supplied regular expression.
   * @param pattern regular expression to match text against
   * @return elements matching the supplied regular expression.
   * @see Element#ownText()
   */
  def getElementsMatchingOwnText(pattern: util.regex.Pattern): Elements = {
    //todo Collector and Evaluator not implemented yet
    //    return Collector.collect(new Evaluator.MatchesOwn(pattern), this)
    null
  }

  /**
   * Find elements whose text matches the supplied regular expression.
   * @param regex regular expression to match text against. You can use <a href="http://java.sun.com/docs/books/tutorial/essential/regex/pattern.html#embedded">embedded flags</a> (such as (?i) and (?m) to control regex options.
   * @return elements matching the supplied regular expression.
   * @see Element#ownText()
   */
  def getElementsMatchingOwnText(regex: String): Elements = {
    var pattern: util.regex.Pattern = null
    try {
      pattern = util.regex.Pattern.compile(regex)
    }
    catch {
      case e: util.regex.PatternSyntaxException =>
        throw new IllegalArgumentException("Pattern syntax error: " + regex, e)
    }
    getElementsMatchingOwnText(pattern)
  }

  /**
   * Find all elements under this element (including self, and children of children).
   *
   * @return all elements
   */
  def getAllElements: Elements = {
    //todo Collector and Evaluator not implemented yet
    //    return Collector.collect(new Evaluator.AllElements, this)
    null
  }

  /**
   * Gets the combined text of this element and all its children. Whitespace is normalized and trimmed.
   * <p>
   * For example, given HTML {{{<p>Hello  <b>there</b> now! </p>}}}, <code>p.text()</code> returns <code> "Hello there now!"</code>
   *
   * @return unencoded text, or empty string if none.
   * @see #ownText()
   * @see #textNodes()
   */
  def text(): String = {
    val accum: java.lang.StringBuilder = new java.lang.StringBuilder
    new NodeTraversor(new NodeVisitor {
      def head(node: Node, depth: Int) {
        node match {
          case t: TextNode =>
            val textNode: TextNode = t.asInstanceOf[TextNode]
            appendNormalisedText(accum, textNode)
          case e: Element =>
            val element: Element = e.asInstanceOf[Element]
            if (accum.length > 0 && (element.isBlock || (element.tag.getName == "br")) && !TextNode.lastCharIsWhitespace(accum)) {
              accum.append(" ")
            }

        }
      }

      def tail(node: Node, depth: Int) {
      }
    }).traverse(this)
    accum.toString.trim
  }

  /**
   * Gets the text owned by this element only; does not get the combined text of all children.
   * <p>
   * For example, given HTML {{{<p>Hello <b>there</b> now!</p>}}}, <code>p.ownText()</code>returns <code>"Hello now!"</code>,
   * whereas <code>p.text()</code> returns <code> "Hello there now!"}}}.</code>
   * Note that the text within the <code>b</code> element is not returned, as it is not a direct child of the <code>p</code> element.
   *
   * @return unencoded text, or empty string if none.
   * @see #text()
   * @see #textNodes()
   */
  def ownText: String = {
    val sb: java.lang.StringBuilder = new java.lang.StringBuilder
    ownText(sb)
    sb.toString.trim
  }

  private def ownText(accum: java.lang.StringBuilder) {
    for (child <- childNodes) {
      child match {
        case t: TextNode =>
          val textNode: TextNode = child.asInstanceOf[TextNode]
          appendNormalisedText(accum, textNode)
        case e: Element =>
          appendWhitespaceIfBr(child.asInstanceOf[Element], accum)
      }
    }
  }

  /**
   * Set the text of this element. Any existing contents (text or elements) will be cleared
   * @param text unencoded text
   * @return this element
   */
  def text(text: String): Element = {
    notNull(text)
    empty
    val textNode: TextNode = new TextNode(text, baseUri)
    appendChild(textNode)
    this
  }

  /**
  Test if this element has any text content (that is not just whitespace).
     @return true if element has non-blank text content.
    */
  def hasText: Boolean = {
    for (child <- childNodes) {
      child match {
        case t: TextNode =>
          val textNode: TextNode = t.asInstanceOf[TextNode]
          if (!textNode.isBlank) {
            return true
          }
        case e: Element =>
          val el: Element = e.asInstanceOf[Element]
          if (el.hasText) {
            return true
          }
      }
    }
    false
  }

  /**
   * Get the combined data of this element. Data is e.g. the inside of a <code>script</code> tag.
   * @return the data, or empty string if none
   *
   * @see #dataNodes()
   */
  def data: String = {
    val sb: StringBuilder = new StringBuilder
    for (childNode <- childNodes) {
      childNode match {
        case d: DataNode =>
          val data: DataNode = d.asInstanceOf[DataNode]
          sb.append(data.getWholeData)
        case e: Element =>
          val element: Element = e.asInstanceOf[Element]
          val elementData: String = element.data
          sb.append(elementData)
      }
    }
    sb.toString()
  }

  /**
   * Gets the literal value of this element's "class" attribute, which may include multiple class names, space
   * separated. (E.g. on <code>&lt;div class="header gray"></code> returns, "<code>header gray</code>")
   * @return The literal class attribute, or <b>empty string</b> if no class attribute set.
   */
  def className: String = attr("class").trim

  /**
   * Get all of the element's class names. E.g. on element {{{<div class="header gray"}>}}},
   * returns a set of two elements <code>"header", "gray"</code. Note that modifications to this set are not pushed to
   * the backing <code>class</code> attribute; use the [[classNames(java.util.Set)]] method to persist them.
   * @return set of classnames, empty if no class attribute
   */
  def classNames: Set[String] = {
    val names: Array[String] = className.split("\\s+")
    val classNames: mutable.Set[String] = mutable.Set(names: _*)
    classNames.remove("")
    classNames.toSet
  }

  /**
  Set the element's <code>class</code> attribute to the supplied class names.
     @param classNames set of classes
  @return this element, for chaining
    */
  def classNames(classNames: Set[String]): Element = {
    notNull(classNames)
    attributes.put("class", Strings.join(classNames, " "))
    this
  }

  /**
   * Tests if this element has a class. Case insensitive.
   * @param className name of class to check for
   * @return true if it does, false if not
   */
  def hasClass(className: String): Boolean = {
    val classNms: Set[String] = classNames
    for (name <- classNms) {
      if (className.equalsIgnoreCase(name)) {
        return true
      }
    }
    false
  }

  /**
  Add a class name to this element's <code>class</code> attribute.
     @param className class name to add
  @return this element
    */
  def addClass(className: String): Element = {
    notNull(className)
    val classes: mutable.Set[String] = mutable.Set(classNames.toArray: _*)
    classes.add(className)
    classNames(classes.toSet)
    this
  }

  /**
   * Remove a class name from this element's <code>class</code> attribute.
   * @param className class name to remove
   * @return this element
   */
  def removeClass(className: String): Element = {
    notNull(className)
    val classes: mutable.Set[String] = mutable.Set(classNames.toArray: _*)
    classes.remove(className)
    classNames(classes.toSet)
    this
  }

  /**
  Toggle a class name on this element's <code>class</code> attribute: if present, remove it; otherwise add it.
     @param className class name to toggle
  @return this element
    */
  def toggleClass(className: String): Element = {
    notNull(className)
    val classes: mutable.Set[String] = mutable.Set(classNames.toArray: _*)
    if (classes.contains(className)) {
      classes.remove(className)
    }
    else {
      classes.add(className)
    }
    classNames(classes.toSet)
    this
  }

  /**
   * Get the value of a form element (input, textarea, etc).
   * @return the value of the form element, or empty string if not set.
   */
  def value: String = if (tagName == "textarea") {
    text
  } else {
    attr("value")
  }

  /**
   * Set the value of a form element (input, textarea, etc).
   * @param value value to set
   * @return this element (for chaining)
   */
  def value(value: String): Element = {
    if (tagName == "textarea") {
      text(value)
    } else {
      attr("value", value)
    }
    this
  }

  private[nodes] def outerHtmlHead(accum: StringBuilder, depth: Int, out: Document.OutputSettings) {
    if (accum.length > 0 &&
            out.prettyPrint &&
            (tagVal.isFormatAsBlock || (parent != null && parent.tagVal.isFormatAsBlock) || out.outline)) {
      indent(accum, depth, out)
    }
    accum.append("<").append(tagName)
    attributes.html(accum, out)
    if (childNodes.isEmpty && tagVal.isSelfClosing) {
      if ((out.syntax eq Document.OutputSettings.Syntax.html) && tagVal.isEmpty) {
        accum.append('>')
      }
      else {
        accum.append(" />")
      }
    }
    else {
      accum.append(">")
    }
  }

  private[nodes] def outerHtmlTail(accum: StringBuilder, depth: Int, out: Document.OutputSettings) {
    if (!(childNodes.isEmpty && tagVal.isSelfClosing)) {
      if (out.prettyPrint && (childNodes.nonEmpty &&
              (tagVal.isFormatAsBlock ||
                      (out.outline &&
                              (childNodes.size > 1 ||
                                      (childNodes.size == 1 && !childNodes.head.isInstanceOf[TextNode])
                                      )
                              )
                      )
              )
      ) {
        indent(accum, depth, out)
      }
      accum.append("</").append(tagName).append(">")
    }
  }

  /**
   * Retrieves the element's inner HTML. E.g. on a <code><div></code> with one empty <code><p></code>, would return
   * <code><p></p></code>. (Whereas 【[[outerHtml()]] would return {{{<div><p></p></div>}}}.)
   *
   * @return String of HTML.
   * @see #outerHtml()
   */
  def html: String = {
    val accum: StringBuilder = new StringBuilder
    html(accum)
    if (getOutputSettings.prettyPrint) {
      accum.toString().trim
    } else {
      accum.toString()
    }
  }

  private def html(accum: scala.StringBuilder) = {
    for (node <- childNodes) {
      node.outerHtml(accum)
    }
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
    this
  }

  override def toString: String = outerHtml

  override def equals(o: Any): Boolean = o match {
    // todo: have nodes hold a child index, compare against that and parent (not children)
    case v: Element => this eq v
    case _ => false
  }

  override def hashCode: Int = 31 * super.hashCode + (if (tag != null) {
    tag.hashCode
  } else {
    0
  })

  override def clone(): Element = super.clone().asInstanceOf[Element]
}

private[nodes] object Element {

  private[nodes] def preserveWhitespace(node: Node): Boolean = {
    if (node != null && node.isInstanceOf[Element]) {
      val element: Element = node.asInstanceOf[Element]
      return element.tagVal.isPreserveWhitespace || element.parent != null && element.parent.tagVal.isPreserveWhitespace
    }
    false
  }

  private def appendNormalisedText(accum: java.lang.StringBuilder, textNode: TextNode) {
    val text: String = textNode.getWholeText
    if (preserveWhitespace(textNode.parentNode)) {
      accum.append(text)
    }
    else {
      Strings.appendNormalisedWhitespace(accum, text, TextNode.lastCharIsWhitespace(accum))
    }
  }

  private def appendWhitespaceIfBr(element: Element, accum: java.lang.StringBuilder) {
    if ((element.tag.getName == "br") && !TextNode.lastCharIsWhitespace(accum)) {
      accum.append(" ")
    }
  }
}
