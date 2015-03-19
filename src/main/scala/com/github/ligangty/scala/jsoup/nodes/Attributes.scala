package com.github.ligangty.scala.jsoup.nodes

import java.util

import com.github.ligangty.scala.jsoup.helper.Validator._

import scala.collection.mutable.ArrayBuffer

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
//  override def iterator: Iterator[Attribute] = ???

  protected val dataPrefix: String = "data-"

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
    if (attributes == null) return ""
    val attr: Attribute = attributes.get(key.toLowerCase)
    if (attr != null) attr.getValue else ""
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
    if (attributes == null) attributes = new util.LinkedHashMap[String, Attribute](2)
    attributes.put(attribute.getKey, attribute)
  }

  /**
  Remove an attribute by key.
     @param key attribute key to remove
    */
  def remove(key: String) {
    notEmpty(key)
    if (attributes == null) return
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
  override def size: Int = {
    if (attributes == null) return 0
    return attributes.size
  }

  /**
   * Add all the attributes from the incoming set to this set.
   * @param incoming attributes to add to these attributes.
   */
  def addAll(incoming: Attributes) {
    if (incoming.size == 0) return
    if (attributes == null) attributes = new util.LinkedHashMap[String, Attribute](incoming.size)
    attributes.putAll(incoming.attributes)
  }

  def iterator: Iterator[Attribute] = {
    return asList.iterator
  }

  /**
   * Get the attributes as a List, for iteration. Do not modify the keys of the attributes via this view, as changes
   * to keys will not be recognised in the containing set.
   * @return an view of the attributes as a List.
   */
  def asList: List[Attribute] = {
    if (attributes == null) return List.empty
    val array: ArrayBuffer[Attribute] = new ArrayBuffer[Attribute](attributes.size)
    import scala.collection.JavaConversions._
    for (entry <- attributes.entrySet) {
      entry match {
        case m: util.Map.Entry[String, Attribute] => array += m.getValue
      }
    }
    return array.toList
  }

  /**
   * Retrieves a filtered view of attributes that are HTML5 custom data attributes; that is, attributes with keys
   * starting with {@code data-}.
   * @return map of custom data attributes.
   */
//  def dataset: Map[String, String] = {
//    return new Attributes#Dataset
//  }
//
//  private class Dataset extends util.AbstractMap[String, String] {
//    private def this() {
//      this()
//      if (attributes == null) attributes = new util.LinkedHashMap[String, Attribute](2)
//    }
//
//    def entrySet: Set[util.Map.Entry[String, String]] = {
//      return new Attributes#Dataset#EntrySet
//    }
//
//    override def put(key: String, value: String): String = {
//      val dataKey: String = dataKey(key)
//      val oldValue: String = if (hasKey(dataKey)) attributes.get(dataKey).getValue else null
//      val attr: Attribute = new Attribute(dataKey, value)
//      attributes.put(dataKey, attr)
//      return oldValue
//    }
//
//    private class EntrySet extends util.AbstractSet[util.Map.Entry[String, String]] {
//      def iterator: Iterator[util.Map.Entry[String, String]] = {
//        return new Attributes#Dataset#DatasetIterator
//      }
//
//      def size: Int = {
//        var count: Int = 0
//        val iter: Iterator[_] = new Attributes#Dataset#DatasetIterator
//        while (iter.hasNext) ({
//          count += 1; count - 1
//        })
//        return count
//      }
//    }
//
//    private class DatasetIterator extends Iterator[util.Map.Entry[String, String]] {
//      private var attrIter: Iterator[Attribute] = attributes.values.iterator
//      private var attr: Attribute = null
//
//      def hasNext: Boolean = {
//        while (attrIter.hasNext) {
//          attr = attrIter.next
//          if (attr.isDataAttribute) return true
//        }
//        return false
//      }
//
//      def next: Map.Entry[String, String] = {
//        return new Attribute(attr.getKey.substring(dataPrefix.length), attr.getValue)
//      }
//
//      def remove {
//        attributes.remove(attr.getKey)
//      }
//    }
//
//  }
}
