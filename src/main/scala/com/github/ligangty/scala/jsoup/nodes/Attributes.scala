package com.github.ligangty.scala.jsoup.nodes

import java.util

import com.github.ligangty.scala.jsoup.helper.Validator._

import scala.collection.JavaConversions._

/**
 * The attributes of an Element.
 * <p/>
 * Attributes are treated as a map: there can be only one value associated with an attribute key.
 * <p/>
 * Attribute key and value comparisons are done case insensitively, and keys are normalised to
 * lower-case.
 *
 */
class Attributes extends Iterable[Attribute] {
  self =>

  // linked hash map to preserve insertion order.
  // null be default as so many elements have no attributes -- saves a good chunk of memory
  private var attributes: util.LinkedHashMap[String, Attribute] = null

  /**
   * Get an attribute value by key.
   * @param key the attribute key
   * @return the attribute value if set; or empty string if not set.
   * @see #hasKey(String)
   */
  def get(key: String): String = {
    notEmpty(key)
    attributes match {
      case attrs if attrs != null =>
        val attr = attrs.get(key.toLowerCase)
        if (attr != null) {
          attr.getValue
        } else {
          ""
        }
      case null => ""
    }
  }

  /**
   * Set a new attribute, or replace an existing one by key.
   * @param key attribute key
   * @param value attribute value
   */
  def put(key: String, value: String) {
    val attr: Attribute = new Attribute(key, value)
    put(attr)
  }

  /**
   * Set a new attribute, or replace an existing one by key.
   * @param attribute attribute
   */
  def put(attribute: Attribute) {
    notNull(attribute)
    if (attributes == null) {
      attributes = new util.LinkedHashMap[String, Attribute](2)
    }
    attributes.put(attribute.getKey, attribute)
  }

  /**
  Remove an attribute by key.
     @param key attribute key to remove
    */
  def remove(key: String) {
    notEmpty(key)
    if (attributes == null) {
      return
    }
    attributes.remove(key.toLowerCase)
  }

  /**
   * Tests if these attributes contain an attribute with this key.
   * @param key key to check for
   * @return true if key exists, false otherwise
   */
  def hasKey(key: String): Boolean = attributes != null && attributes.containsKey(key.toLowerCase)

  /**
  Get the number of attributes in this set.
     @return size
    */
  override def size: Int = if (attributes == null) {
    0
  } else {
    attributes.size
  }

  /**
   * Add all the attributes from the incoming set to this set.
   * @param incoming attributes to add to these attributes.
   */
  def addAll(incoming: Attributes) {
    if (incoming.size == 0) {
      return
    }
    if (attributes == null) {
      attributes = new util.LinkedHashMap[String, Attribute](incoming.size)
    }
    attributes.putAll(incoming.attributes)
  }

  def iterator: Iterator[Attribute] = asList.iterator

  /**
   * Get the attributes as a List, for iteration. Do not modify the keys of the attributes via this view, as changes
   * to keys will not be recognised in the containing set.
   * @return an view of the attributes as a List.
   */
  def asList: List[Attribute] = {
    import scala.collection.JavaConversions._
    if (attributes == null) {
      List.empty
    }
    else {
      attributes.entrySet().map(entry => entry.getValue).toList
    }
  }

  /**
   * Retrieves a filtered view of attributes that are HTML5 custom data attributes; that is, attributes with keys
   * starting with <code>data-</code>.
   * @return map of custom data attributes.
   */
  def dataset: Map[String, String] = new self.Dataset().toMap

  /**
  Get the HTML representation of these attributes.
     @return HTML
    */
  def html: String = {
    val accum: StringBuilder = new StringBuilder
    html(accum, new Document("").outputSettings)
    accum.toString()
  }

  private[nodes] def html(accum: StringBuilder, out: Document.OutputSettings) {
    if (attributes == null) {
      return
    }
    import scala.collection.JavaConversions._
    for (entry <- attributes.entrySet) {
      val attribute: Attribute = entry.getValue
      accum.append(" ")
      attribute.html(accum, out)
    }
  }

  override def toString(): String = html

  override def equals(o: Any): Boolean = o match {
    case attrs: Attributes => (this eq attrs) || !(if (attributes != null) {
      !(attributes == attrs.attributes)
    } else {
      attrs.attributes != null
    })
    case _ => false
  }

  override def hashCode: Int = if (attributes != null) {
    attributes.hashCode
  } else {
    0
  }

  override def clone: Attributes = {
    if (attributes == null) {
      return new Attributes
    }
    var clone: Attributes = null
    try {
      clone = super.clone.asInstanceOf[Attributes]
    } catch {
      case e: CloneNotSupportedException =>
        throw new RuntimeException(e)
    }
    clone.attributes = new util.LinkedHashMap[String, Attribute](attributes.size)
    this.foreach(attribute => clone.attributes.put(attribute.getKey, attribute.clone))
    clone
  }

  private[Attributes] class Dataset(u: Unit = ()) extends util.AbstractMap[String, String] {
    self =>

    private def this() {
      this(())
      if (attributes == null) {
        attributes = new util.LinkedHashMap[String, Attribute](2)
      }
    }

    override def entrySet: util.Set[util.Map.Entry[String, String]] = new self.EntrySet()

    override def put(key: String, value: String): String = {
      val datKey: String = Attributes.dataKey(key)
      val oldValue: String = if (hasKey(datKey)) {
        attributes.get(datKey).getValue
      } else {
        null
      }
      val attr: Attribute = new Attribute(datKey, value)
      attributes.put(datKey, attr)
      oldValue
    }

    private[Dataset] class EntrySet extends util.AbstractSet[util.Map.Entry[String, String]] {

      override def iterator: util.Iterator[util.Map.Entry[String, String]] = new self.DatasetIterator

      override def size: Int = {
        var count: Int = 0
        val iter: Iterator[_] = new DatasetIterator
        while (iter.hasNext) {
          count += 1
        }
        count
      }
    }

    private[Dataset] class DatasetIterator extends util.Iterator[util.Map.Entry[String, String]] {

      private var attrIter: Iterator[Attribute] = attributes.values.iterator
      private var attr: Attribute = null

      def hasNext: Boolean = {
        while (attrIter.hasNext) {
          attr = attrIter.next()
          if (attr.isDataAttribute) {
            return true
          }
        }
        false
      }

      def next: util.Map.Entry[String, String] = new Attribute(attr.getKey.substring(Attributes.dataPrefix.length), attr.getValue)

      def remove() = attributes.remove(attr.getKey)
    }

  }

}

private[nodes] object Attributes {

  private[nodes] val dataPrefix: String = "data-"

  def dataKey(key: String) = dataPrefix + key
}

