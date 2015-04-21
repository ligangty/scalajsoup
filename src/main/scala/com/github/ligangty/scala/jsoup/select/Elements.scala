package com.github.ligangty.scala.jsoup.select

import com.github.ligangty.scala.jsoup.helper.Validator._
import com.github.ligangty.scala.jsoup.nodes.{FormElement, Element}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * A list of [[Element]]s, with methods that act on every element in the list.
 * <p/>
 * To get an <code>Elements</code> object, use the [[Element.select(String)]] method.
 */
class Elements private(u: Unit = ()) extends mutable.Seq[Element] with Cloneable {

  private var contents: mutable.Buffer[Element] = null

  def this() {
    this(())
    contents = ArrayBuffer[Element]()
  }

  def this(initialCapacity: Int) {
    this(())
    contents = new ArrayBuffer[Element](initialCapacity)
  }

  def this(elements: Traversable[Element]) {
    this()
    contents = ArrayBuffer() ++= elements
  }

  def this(elements: mutable.Buffer[Element]) {
    this()
    contents = elements
  }

  def this(elements: Element*) {
    this(elements.toBuffer)
  }

  /**
   * Creates a deep copy of these elements.
   * @return a deep copy
   */
  override def clone(): Elements = {
    var clone: Elements = null
    try {
      clone = super.clone().asInstanceOf[Elements]
    } catch {
      case e: CloneNotSupportedException => throw new RuntimeException(e)
    }
    val elements: mutable.Buffer[Element] = new mutable.ArrayBuffer[Element]()
    clone.contents = elements
    for (e <- contents) {
      elements.append(e.clone())
    }
    clone
  }

  // attribute methods
  /**
   * Get an attribute value from the first matched element that has the attribute.
   * @param attributeKey The attribute key.
   * @return The attribute value from the first matched element that has the attribute.. If no elements were matched (isEmpty() == true),
   *         or if the no elements have the attribute, returns empty string.
   * @see #hasAttr(String)
   */
  def attr(attributeKey: String): String = {
    val founded = contents.filter(_.hasAttr(attributeKey))
    if (founded.nonEmpty) {
      founded.head.attr(attributeKey)
    } else {
      ""
    }
  }

  /**
   * Checks if any of the matched elements have this attribute set.
   * @param attributeKey attribute key
   * @return true if any of the elements have the attribute; false if none do.
   */
  def hasAttr(attributeKey: String): Boolean = contents.exists(_.hasAttr(attributeKey))

  /**
   * Set an attribute on all matched elements.
   * @param attributeKey attribute key
   * @param attributeValue attribute value
   * @return this
   */
  def attr(attributeKey: String, attributeValue: String): Elements = {
    contents.foreach(_.attr(attributeKey, attributeValue))
    this
  }

  /**
   * Remove an attribute from every matched element.
   * @param attributeKey The attribute to remove.
   * @return this (for chaining)
   */
  def removeAttr(attributeKey: String): Elements = {
    contents.foreach(_.removeAttr(attributeKey))
    this
  }

  /**
   * Add the class name to every matched element's <code>class</code> attribute.
   * @param className class name to add
   * @return this
   */
  def addClass(className: String): Elements = {
    contents.foreach(_.addClass(className))
    this
  }

  /**
   * Remove the class name from every matched element's <code>class</code> attribute, if present.
   * @param className class name to remove
   * @return this
   */
  def removeClass(className: String): Elements = {
    contents.foreach(_.removeClass(className))
    this
  }

  /**
   * Toggle the class name on every matched element's <code>class</code> attribute.
   * @param className class name to add if missing, or remove if present, from every element.
   * @return this
   */
  def toggleClass(className: String): Elements = {
    contents.foreach(_.toggleClass(className))
    this
  }

  /**
   * Determine if any of the matched elements have this class name set in their <code>class</code> attribute.
   * @param className class name to check for
   * @return true if any do, false if none do
   */
  def hasClass(className: String): Boolean = contents.filter(_.hasClass(className)).nonEmpty

  /**
   * Get the form element's value of the first matched element.
   * @return The form element's value, or empty if not set.
   * @see Element#val()
   */
  def value: String = if (size > 0) {
    first().value
  } else {
    ""
  }

  /**
   * Set the form element's value in each of the matched elements.
   * @param value The value to set into each matched element
   * @return this (for chaining)
   */
  def value(value: String): Elements = {
    contents.foreach(_.value(value))
    this
  }

  /**
   * Get the combined text of all the matched elements.
   * <p>
   * Note that it is possible to get repeats if the matched elements contain both parent elements and their own
   * children, as the Element.text() method returns the combined text of a parent and all its children.
   * @return string of all text: unescaped and no HTML.
   * @see Element#text()
   */
  def text: String = contents.map(_.text()).mkString(" ")

  def hasText: Boolean = contents.exists(_.hasText)

  /**
   * Get the combined inner HTML of all matched elements.
   * @return string of all element's inner HTML.
   * @see #text()
   * @see #outerHtml()
   */
  def html: String = contents.map(_.html).mkString("\n")

  /**
   * Get the combined outer HTML of all matched elements.
   * @return string of all element's outer HTML.
   * @see #text()
   * @see #html()
   */
  def outerHtml: String = contents.map(_.outerHtml).mkString("\n")

  /**
   * Get the combined outer HTML of all matched elements. Alias of [[outerHtml()]].
   * @return string of all element's outer HTML.
   * @see #text()
   * @see #html()
   */
  override def toString(): String = outerHtml

  /**
   * Update the tag name of each matched element. For example, to change each <code>&lt;i&gt;</code> to a <code>&lt;em&gt;</code>, do
   * <code>doc.select("i").tagName("em");</code>
   * @param tagName the new tag name
   * @return this, for chaining
   * @see Element#tagName(String)
   */
  def tagName(tagName: String): Elements = {
    contents.foreach(_.tagName(tagName))
    this
  }

  /**
   * Set the inner HTML of each matched element.
   * @param html HTML to parse and set into each matched element.
   * @return this, for chaining
   * @see Element#html(String)
   */
  def html(html: String): Elements = {
    contents.foreach(_.html(html))
    this
  }

  /**
   * Add the supplied HTML to the start of each matched element's inner HTML.
   * @param html HTML to add inside each element, before the existing HTML
   * @return this, for chaining
   * @see Element#prepend(String)
   */
  def prepend(html: String): Elements = {
    contents.foreach(_.prepend(html))
    this
  }

  /**
   * Add the supplied HTML to the end of each matched element's inner HTML.
   * @param html HTML to add inside each element, after the existing HTML
   * @return this, for chaining
   * @see Element#append(String)
   */
  def append(html: String): Elements = {
    contents.foreach(_.append(html))
    this
  }

  /**
   * Insert the supplied HTML before each matched element's outer HTML.
   * @param html HTML to insert before each element
   * @return this, for chaining
   * @see Element#before(String)
   */
  def before(html: String): Elements = {
    contents.foreach(_.before(html))
    this
  }

  /**
   * Insert the supplied HTML after each matched element's outer HTML.
   * @param html HTML to insert after each element
   * @return this, for chaining
   * @see Element#after(String)
   */
  def after(html: String): Elements = {
    contents.foreach(_.after(html))
    this
  }

  /**
   * Wrap the supplied HTML around each matched elements. For example, with HTML
   * {{{<p><b>This</b> is <b>Jsoup</b></p>}}},
   * <code>doc.select("b").wrap("&lt;i&gt;&lt;/i&gt;");</code>
   * becomes {{{<p><i><b>This</b></i> is <i><b>jsoup</b></i></p>}}}
   * @param html HTML to wrap around each element, e.g. { @code <div class="head"></div>}. Can be arbitrarily deep.
   * @return this (for chaining)
   * @see Element#wrap
   */
  def wrap(html: String): Elements = {
    notEmpty(html)
    contents.foreach(_.wrap(html))
    this
  }

  /**
   * Removes the matched elements from the DOM, and moves their children up into their parents. This has the effect of
   * dropping the elements but keeping their children.
   * <p/>
   * This is useful for e.g removing unwanted formatting elements but keeping their contents.
   * <p/>
   * E.g. with HTML: {{{<div><font>One</font> <font><a href="/">Two</a></font></div>}}}<br/>
   * <code>doc.select("font").unwrap();</code><br/>
   * HTML = {{{<div>One <a href="/">Two</a></div>}}}
   *
   * @return this (for chaining)
   * @see Node#unwrap
   */
  def unwrap: Elements = {
    contents.foreach(_.unwrap)
    this
  }

  /**
   * Empty (remove all child nodes from) each matched element. This is similar to setting the inner HTML of each
   * element to nothing.
   * <p>
   * E.g. HTML: {{{<div><p>Hello <b>there</b></p> <p>now</p></div>}}}<br>
   * <code>doc.select("p").empty();</code><br>
   * HTML = {{{<div><p></p> <p></p></div>}}}
   * @return this, for chaining
   * @see Element#empty()
   * @see #remove()
   */
  def empty: Elements = {
    contents.foreach(_.empty)
    this
  }

  /**
   * Remove each matched element from the DOM. This is similar to setting the outer HTML of each element to nothing.
   * <p>
   * E.g. HTML: {{{<div><p>Hello</p> <p>there</p> <img /></div>}}}<br>
   * <code>doc.select("p").remove();</code><br>
   * HTML = {{{<div> <img /></div>}}}
   * <p>
   * Note that this method should not be used to clean user-submitted HTML; rather, use [[com.github.ligangty.scala.safety.Cleaner]] to clean HTML.
   * @return this, for chaining
   * @see Element#empty()
   * @see #empty()
   */
  def remove: Elements = {
    contents.foreach(_.remove())
    this
  }

  // filters
  /**
   * Find matching elements within this element list.
   * @param query A { @link Selector} query
   * @return the filtered list of elements, or an empty list if none match.
   */
  def select(query: String): Elements = {
    Selector.select(query, this)
  }

  /**
   * Remove elements from this list that match the [[Selector]] query.
   * <p>
   * E.g. HTML: {{{<div class=logo>One</div> <div>Two</div>}}}<br>
   * <code>Elements divs = doc.select("div").not("#logo");</code><br>
   * Result: {{{divs: [<div>Two</div>]}}}
   * <p>
   * @param query the selector query whose results should be removed from these elements
   * @return a new elements list that contains only the filtered results
   */
  def not(query: String): Elements = {
    val out: Elements = Selector.select(query, this)
    Selector.filterOut(this, out)
  }

  /**
   * Get the <i>nth</i> matched element as an Elements object.
   * <p>
   * See also [[get(int)]] to retrieve an Element.
   * @param index the (zero-based) index of the element in the list to retain
   * @return Elements containing only the specified element, or, if that element did not exist, an empty list.
   */
  def eq(index: Int): Elements = if (contents.size > index) {
    new Elements(get(index))
  } else {
    new Elements
  }

  /**
   * Test if any of the matched elements match the supplied query.
   * @param query A selector
   * @return true if at least one element in the list matches the query.
   */
  def is(query: String): Boolean = !select(query).isEmpty

  /**
   * Get all of the parents and ancestor elements of the matched elements.
   * @return all of the parents and ancestor elements of the matched elements
   */
  def parents: Elements = {
    var combo: mutable.Set[Element] = new mutable.LinkedHashSet[Element]
    for (e <- contents) {
      combo ++= e.parents
    }
    new Elements(combo)
  }

  // list-like methods
  /**
   * Get the first matched element.
   * @return The first matched element, or <code>null</code> if contents is empty.
   */
  def first(): Element = if (contents.isEmpty) {
    null
  } else {
    contents.head
  }

  /**
   * Get the last matched element.
   * @return The last matched element, or <code>null</code> if contents is empty.
   */
  override def last: Element = if (contents.isEmpty) {
    null
  } else {
    contents.last
  }

  /**
   * Perform a depth-first traversal on each of the selected elements.
   * @param nodeVisitor the visitor callbacks to perform on each node
   * @return this, for chaining
   */
  def traverse(nodeVisitor: NodeVisitor): Elements = {
    notNull(nodeVisitor)
    val traversor: NodeTraversor = new NodeTraversor(nodeVisitor)
    for (el <- contents) {
      traversor.traverse(el)
    }
    this
  }

  /**
   * Get the [[com.github.ligangty.scala.jsoup.nodes.FormElement]] forms from the selected elements, if any.
   * @return a list of { @link FormElement}s pulled from the matched elements. The list will be empty if the elements contain
   *         no forms.
   */
  def forms: Seq[FormElement] = {
    contents.filter(_.isInstanceOf[FormElement]).map(_.asInstanceOf[FormElement])
  }

  // implements List<Element> delegates:
  override def size: Int = contents.size

  override def isEmpty: Boolean = contents.isEmpty

  override def contains(elem: Any): Boolean = contents.contains(elem)

  override def iterator: Iterator[Element] = contents.iterator

  def toArray: Array[AnyRef] = contents.toArray

  def add(element: Element): Boolean = {
    contents.append(element)
    true
  }

  def add(index: Int, element: Element): Boolean = {
    contents.insert(index, element)
    true
  }

  def remove(o: AnyRef): Boolean = {
    val finded = contents.indexOf(o)
    if (finded >= 0) {
      contents.remove(finded)
      true
    } else {
      false
    }
  }

  def containsAll(c: Traversable[_]): Boolean = {
    for (e <- c) {
      if (!contents.contains(e)) {
        return false
      }
    }
    true
  }

  def addAll(c: Traversable[_ <: Element]): Boolean = {
    contents ++= c
    c.size != 0
  }

  def clear(): Unit = contents.clear()

  override def equals(o: Any): Boolean = contents == o

  override def hashCode(): Int = contents.hashCode()

  def get(index: Int): Element = contents(index)

  def remove(index: Int): Element = contents.remove(index)

  override def indexOf[A >: Element](o: A): Int = {
    contents.indexOf(o)
  }

  override def lastIndexOf[A >: Element](o: A): Int = {
    contents.lastIndexOf(o)
  }

  //  def listIterator: util.ListIterator[Element] = {
  //    contents.listIterator
  //  }
  //
  //  def listIterator(index: Int): util.ListIterator[Element] = {
  //    contents.listIterator(index)
  //  }
  //
  //  def subList(fromIndex: Int, toIndex: Int): util.List[Element] = {
  //    contents.subList(fromIndex, toIndex).toList
  //  }

  override def update(idx: Int, elem: Element): Unit = contents.update(idx, elem)

  override def apply(idx: Int): Element = contents(idx)

  override def length: Int = contents.size
}
