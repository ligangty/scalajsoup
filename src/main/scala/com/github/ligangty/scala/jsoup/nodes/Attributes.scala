package com.github.ligangty.scala.jsoup.nodes

import java.util

import com.github.ligangty.scala.jsoup.helper.Validator._

import scala.collection.JavaConversions._
import scala.collection.mutable

/**
 * The attributes of an Element.
 * <p>
 * Attributes are treated as a map: there can be only one value associated with an attribute key.
 * </p>
 * <p>
 * Attribute key and value comparisons are done case insensitively, and keys are normalised to
 * lower-case.
 * </p>
 */
class Attributes extends Iterable[Attribute] with Cloneable {
  self =>

  // linked hash map to preserve insertion order.
  // null be default as so many elements have no attributes -- saves a good chunk of memory
  private var attributes: mutable.LinkedHashMap[String, Attribute] = null

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
        attr match {
          case None => ""
          case Some(a) => a.getValue
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
      attributes = mutable.LinkedHashMap[String, Attribute]()
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
      attributes = mutable.LinkedHashMap[String, Attribute]()
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
    if (attributes == null) {
      List.empty
    } else {
      val a = attributes.map(e => e._2)
      a.toList
    }
  }

  /**
   * Retrieves a filtered view of attributes that are HTML5 custom data attributes; that is, attributes with keys
   * starting with <code>data-</code>.
   * @return map of custom data attributes.
   */
  def dataset: mutable.Map[String, String] = new self.Dataset()

  /**
   * Get the HTML representation of these attributes.
   * @return HTML
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

  /**
   * Checks if these attributes are equal to another set of attributes, by comparing the two sets
   * @param o attributes to compare with
   * @return if both sets of attributes have the same content
   */
  override def equals(o: Any): Boolean = o match {
    case attrs: Attributes => (this eq attrs) || !(if (attributes != null) {
      !(attributes == attrs.attributes)
    } else {
      attrs.attributes != null
    })
    case _ => false
  }

  /**
   * Calculates the hashcode of these attributes, by iterating all attributes and summing their hashcodes.
   * @return calculated hashcode
   */
  override def hashCode: Int = if (attributes != null) {
    attributes.hashCode()
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
    clone.attributes = mutable.LinkedHashMap[String, Attribute]()
    this.foreach(attribute => clone.attributes.put(attribute.getKey, attribute.clone))
    clone
  }

  private[Attributes] class Dataset private[Attributes]() extends mutable.Map[String, String] {
    self =>
    if (attributes == null) attributes = mutable.LinkedHashMap[String, Attribute]()

    override def +=(kv: (String, String)): this.type = {
      val datKey: String = Attributes.dataKey(kv._1)
      val attr: Attribute = new Attribute(datKey, kv._2.toString)
      attributes.put(datKey, attr)
      this
    }

    override def -=(key: String): this.type = {
      remove(key)
      this
    }

    override def put(key: String, value: String): Option[String] = {
      val oldValue = get(key)
      val datKey: String = Attributes.dataKey(key.toString)
      val attr: Attribute = new Attribute(datKey, value.toString)
      attributes.put(datKey, attr)
      oldValue
    }

    override def get(key: String): Option[String] = {
      val datKey: String = Attributes.dataKey(key.toString)
      if (hasKey(datKey)) {
        val attr = attributes(datKey)
        if(attr.isDataAttribute){
          Some(attr.getValue)
        }else {
          None
        }
      } else {
        None
      }
    }

    override def remove(key: String): Option[String] = {
      val datKey: String = Attributes.dataKey(key)
      if (hasKey(datKey)) {
        val attr = attributes(datKey)
        if(attr.isDataAttribute){
          Some(attributes.remove(datKey).get.getValue)
        }else {
          None
        }
      } else {
        None
      }
    }

    override def size: Int = {
      var count: Int = 0
      val iter: Iterator[_] = this.iterator
      while (iter.hasNext) {
        count += 1
      }
      count
    }

    override def iterator: Iterator[(String, String)] = new Iterator[(String, String)] {
      private var attrIter: Iterator[Attribute] = attributes.values.iterator
      private var attr: Attribute = null

      override def next(): (String, String) = new Attribute(attr.getKey.substring(Attributes.dataPrefix.length), attr.getValue)

      override def hasNext: Boolean = {
        while (attrIter.hasNext) {
          attr = attrIter.next()
          if (attr.isDataAttribute) {
            return true
          }
        }
        false
      }
    }
  }

  //  private[Attributes] class Dataset private[Attributes]() extends mutable.HashMap[String, String] {
  //    self =>
  //    if(attributes==null) attributes = mutable.LinkedHashMap[String, Attribute]()
  //
  //    def entrySet: mutable.Set[Product2[String, String]] = new self.EntrySet
  //
  //    override def put(key: String, value: String): Option[String] = {
  //      val datKey: String = Attributes.dataKey(key.toString)
  //      val oldValue: String = if (hasKey(datKey)) {
  //        attributes.get(datKey).get.getValue
  //      } else {
  //        null
  //      }
  //      val attr: Attribute = new Attribute(datKey, value.toString)
  //      attributes.put(datKey, attr)
  //      Some(oldValue)
  //    }
  //
  //    private[Dataset] class EntrySet extends mutable.HashSet[Product2[String,String]] {
  //
  //      override def iterator: Iterator[Product2[String, String]] = new self.DatasetIterator
  //
  //      override def size: Int = {
  //        var count: Int = 0
  //        val iter: Iterator[_] = new DatasetIterator
  //        while (iter.hasNext) {
  //          count += 1
  //        }
  //        count
  //      }
  //    }
  //
  //    private[Dataset] class DatasetIterator extends Iterator[Product2[String, String]] {
  //
  //      private var attrIter: Iterator[Attribute] = attributes.values.iterator
  //      private var attr: Attribute = null
  //
  //      override def hasNext: Boolean = {
  //        while (attrIter.hasNext) {
  //          attr = attrIter.next()
  //          if (attr.isDataAttribute) {
  //            return true
  //          }
  //        }
  //        false
  //      }
  //
  //      override def next(): Product2[String, String] = new Attribute(attr.getKey.substring(Attributes.dataPrefix.length), attr.getValue)
  //
  //      def remove() = attributes.remove(attr.getKey)
  //    }
  //
  //  }

}

private[nodes] object Attributes {

  private[nodes] val dataPrefix: String = "data-"

  def dataKey(key: String) = dataPrefix + key
}

