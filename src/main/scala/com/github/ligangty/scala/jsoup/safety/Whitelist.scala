package com.github.ligangty.scala.jsoup.safety

import com.github.ligangty.scala.jsoup.helper.Validator._
import com.github.ligangty.scala.jsoup.nodes.{Attributes, Attribute, Element}

import scala.collection.mutable

import Whitelist._

/**
 * Whitelists define what HTML (elements and attributes) to allow through the cleaner.Everything else is removed.
 * <p>
 * Start with one of the defaults:
 * </p>
 * <ul>
 * <li>{@link#none}</li>
 * <li>{@link#simpleText}</li>
 * <li>{@link#basic}</li>
 * <li>{@link#basicWithImages}</li>
 * <li>{@link#relaxed}</li>
 * </ul>
 * <p>
 * If you need to allow more through (please be careful!), tweak a base whitelist with:
 * </p>
 * <ul>
 * <li>{@link#addTags}</li>
 * <li>{@link#addAttributes}</li>
 * <li>{@link#addEnforcedAttribute}</li>
 * <li>{@link#addProtocols}</li>
 * </ul>
 * <p>
 * You can remove any setting from an existing whitelist ith:
 * </p>
 * <ul>
 * <li>{@link#removeTags}</li>
 * <li>{@link#removeAttributes}</li>
 * <li>{@link#removeEnforcedAttribute}</li>
 * <li>{@link#removeProtocols}</li>
 * </ul>
 *
 * <p>
 * The cleaner and these whitelists assume that you want to clean a
 * <code>
 * body
 * </code>
 * fragment of HTML (to add user
 * supplied HTML into a templated page), and not to clean a full HTML document. If the latter is the case, either wrap the
 * document HTML around the cleaned body HTML, or create a whitelist that allows
 * <code>
 * html
 * </code>
 * and
 * <code>
 * head
 * </code>
 * elements as appropriate.
 * </p>
 * <p>
 * If you are going to extend a whitelist, please be very careful. Make sure you understand what attributes may lead to
 * XSS attack vectors. URL attributes are particularly vulnerable and require careful validation. See
 * http://ha.ckers.org/xss.html for some XSS attack examples.
 * </p>
 * @constructor Create a new, empty whitelist. Generally it will be better to start with a default prepared whitelist instead.
 */
class Whitelist {

  private var tagNames = new mutable.HashSet[Whitelist.TagName]
  private var attributes = new mutable.HashMap[Whitelist.TagName, mutable.Set[Whitelist.AttributeKey]]
  private var enforcedAttributes = new mutable.HashMap[Whitelist.TagName, mutable.Map[Whitelist.AttributeKey, Whitelist.AttributeValue]]
  private var protocols = new mutable.HashMap[Whitelist.TagName, mutable.Map[Whitelist.AttributeKey, mutable.Set[Whitelist.Protocol]]]
  private var preserveRelativeLinks = false

  /**
  Add a list of allowed elements to a whitelist. (If a tag is not allowed, it will be removed from the HTML.)

     @param tags tag names to allow
  @return this (for chaining)
    */
  def addTags(tags: String*): Whitelist = {
    notNull(tags)
    for (tagName <- tags) {
      notEmpty(tagName)
      tagNames.add(TagName.valueOf(tagName))
    }
    this
  }

  /**
   * Remove a list of allowed elements from a whitelist. (If a tag is not allowed, it will be removed from the HTML.)
   *
   * @param tags tag names to disallow
   * @return this (for chaining)
   */
  def removeTags(tags: String*): Whitelist = {
    notNull(tags)
    for (tag <- tags) {
      notEmpty(tag)
      val tagName: Whitelist.TagName = TagName.valueOf(tag)
      if (tagNames.remove(tagName)) {
        attributes.remove(tagName)
        enforcedAttributes.remove(tagName)
        protocols.remove(tagName)
      }
    }
    this
  }

  /**
   * Add a list of allowed attributes to a tag. (If an attribute is not allowed on an element, it will be removed.)
   * <p>
   * E.g.: <code>addAttributes("a", "href", "class")</code> allows <code>href</code> and <code>class</code> attributes
   * on <code>a</code> tags.
   * </p>
   * <p>
   * To make an attribute valid for <b>all tags</b>, use the pseudo tag <code>:all</code>, e.g.
   * <code>addAttributes(":all", "class")</code>.
   * </p>
   *
   * @param tag  The tag the attributes are for. The tag will be added to the allowed tag list if necessary.
   * @param keys List of valid attributes for the tag
   * @return this (for chaining)
   */
  def addAttributes(tag: String, keys: String*): Whitelist = {
    notEmpty(tag)
    notNull(keys)
    isTrue(keys.length > 0, "No attributes supplied.")
    val tagName: Whitelist.TagName = TagName.valueOf(tag)
    if (!tagNames.contains(tagName)) {
      tagNames.add(tagName)
    }
    val attributeSet: mutable.Set[Whitelist.AttributeKey] = new mutable.HashSet[Whitelist.AttributeKey]
    for (key <- keys) {
      notEmpty(key)
      attributeSet += AttributeKey.valueOf(key)
    }
    if (attributes.contains(tagName)) {
      val currentSet: mutable.Set[Whitelist.AttributeKey] = attributes(tagName)
      currentSet ++= attributeSet
    } else {
      attributes.put(tagName, attributeSet)
    }
    this
  }

  /**
   * Remove a list of allowed attributes from a tag. (If an attribute is not allowed on an element, it will be removed.)
   * <p>
   * E.g.: <code>removeAttributes("a", "href", "class")</code> disallows <code>href</code> and <code>class</code>
   * attributes on <code>a</code> tags.
   * </p>
   * <p>
   * To make an attribute invalid for <b>all tags</b>, use the pseudo tag <code>:all</code>, e.g.
   * <code>removeAttributes(":all", "class")</code>.
   * </p>
   *
   * @param tag  The tag the attributes are for.
   * @param keys List of invalid attributes for the tag
   * @return this (for chaining)
   */
  def removeAttributes(tag: String, keys: String*): Whitelist = {
    notEmpty(tag)
    notNull(keys)
    isTrue(keys.length > 0, "No attributes supplied.")
    val tagName: Whitelist.TagName = TagName.valueOf(tag)
    val attributeSet: mutable.Set[Whitelist.AttributeKey] = new mutable.HashSet[Whitelist.AttributeKey]
    for (key <- keys) {
      notEmpty(key)
      attributeSet += AttributeKey.valueOf(key)
    }
    if (tagNames.contains(tagName) && attributes.contains(tagName)) {
      val currentSet: mutable.Set[Whitelist.AttributeKey] = attributes(tagName)
      currentSet --= attributeSet
      if (currentSet.isEmpty) {
        attributes.remove(tagName)
      }
    }
    if (tag == ":all") {
      for (name <- attributes.keySet) {
        val currentSet: mutable.Set[Whitelist.AttributeKey] = attributes(name)
        currentSet --= attributeSet
        if (currentSet.isEmpty) {
          attributes.remove(name)
        }
      }
    }
    this
  }

  /**
   * Add an enforced attribute to a tag. An enforced attribute will always be added to the element. If the element
   * already has the attribute set, it will be overridden.
   * <p>
   * E.g.: <code>addEnforcedAttribute("a", "rel", "nofollow")</code> will make all <code>a</code> tags output as
   * <code>&lt;a href="..." rel="nofollow"&gt;</code>
   * </p>
   *
   * @param tag   The tag the enforced attribute is for. The tag will be added to the allowed tag list if necessary.
   * @param key   The attribute key
   * @param value The enforced attribute value
   * @return this (for chaining)
   */
  def addEnforcedAttribute(tag: String, key: String, value: String): Whitelist = {
    notEmpty(tag)
    notEmpty(key)
    notEmpty(value)
    val tagName: Whitelist.TagName = TagName.valueOf(tag)
    if (!tagNames.contains(tagName)) {
      tagNames.add(tagName)
    }
    val attrKey: Whitelist.AttributeKey = AttributeKey.valueOf(key)
    val attrVal: Whitelist.AttributeValue = AttributeValue.valueOf(value)
    if (enforcedAttributes.contains(tagName)) {
      enforcedAttributes(tagName).put(attrKey, attrVal)
    } else {
      val attrMap: mutable.Map[Whitelist.AttributeKey, Whitelist.AttributeValue] = new mutable.HashMap[Whitelist.AttributeKey, Whitelist.AttributeValue]
      attrMap.put(attrKey, attrVal)
      enforcedAttributes.put(tagName, attrMap)
    }
    this
  }

  /**
   * Remove a previously configured enforced attribute from a tag.
   *
   * @param tag   The tag the enforced attribute is for.
   * @param key   The attribute key
   * @return this (for chaining)
   */
  def removeEnforcedAttribute(tag: String, key: String): Whitelist = {
    notEmpty(tag)
    notEmpty(key)
    val tagName: Whitelist.TagName = TagName.valueOf(tag)
    if (tagNames.contains(tagName) && enforcedAttributes.contains(tagName)) {
      val attrKey: Whitelist.AttributeKey = AttributeKey.valueOf(key)
      val attrMap: mutable.Map[Whitelist.AttributeKey, Whitelist.AttributeValue] = enforcedAttributes(tagName)
      attrMap.remove(attrKey)
      if (attrMap.isEmpty) {
        enforcedAttributes.remove(tagName)
      }
    }
    this
  }

  /**
   * Configure this Whitelist to preserve relative links in an element's URL attribute, or convert them to absolute
   * links. By default, this is <b>false</b>: URLs will be  made absolute (e.g. start with an allowed protocol, like
   * e.g. {@code http://}.
   * <p>
   * Note that when handling relative links, the input document must have an appropriate {@code base URI} set when
   * parsing, so that the link's protocol can be confirmed. Regardless of the setting of the {@code preserve relative
     * links} option, the link must be resolvable against the base URI to an allowed protocol; otherwise the attribute
   * will be removed.
   * </p>
   *
   * @param preserve { @code true} to allow relative links, { @code false} (default) to deny
   * @return this Whitelist, for chaining.
   * @see #addProtocols
   */
  def preserveRelativeLinks(preserve: Boolean): Whitelist = {
    preserveRelativeLinks = preserve
    this
  }

  /**
   * Add allowed URL protocols for an element's URL attribute. This restricts the possible values of the attribute to
   * URLs with the defined protocol.
   * <p>
   * E.g.: <code>addProtocols("a", "href", "ftp", "http", "https")</code>
   * </p>
   * <p>
   * To allow a link to an in-page URL anchor (i.e. <code>&lt;a href="#anchor"&gt;</code>, add a <code>#</code>:<br>
   * E.g.: <code>addProtocols("a", "href", "#")</code>
   * </p>
   *
   * @param tag       Tag the URL protocol is for
   * @param key       Attribute key
   * @param protocols List of valid protocols
   * @return this, for chaining
   */
  def addProtocols(tag: String, key: String, protocols: String*): Whitelist = {
    notEmpty(tag)
    notEmpty(key)
    notNull(protocols)
    val tagName: Whitelist.TagName = TagName.valueOf(tag)
    val attrKey: Whitelist.AttributeKey = AttributeKey.valueOf(key)
    var attrMap: mutable.Map[Whitelist.AttributeKey, mutable.Set[Whitelist.Protocol]] = null
    var protSet: mutable.Set[Whitelist.Protocol] = null
    if (this.protocols.contains(tagName)) {
      attrMap = this.protocols(tagName)
    } else {
      attrMap = new mutable.HashMap[Whitelist.AttributeKey, mutable.Set[Whitelist.Protocol]]
      this.protocols.put(tagName, attrMap)
    }
    if (attrMap.contains(attrKey)) {
      protSet = attrMap(attrKey)
    } else {
      protSet = new mutable.HashSet[Whitelist.Protocol]
      attrMap.put(attrKey, protSet)
    }
    for (protocol <- protocols) {
      notEmpty(protocol)
      val prot: Whitelist.Protocol = Protocol.valueOf(protocol)
      protSet += prot
    }
    this
  }

  /**
   * Remove allowed URL protocols for an element's URL attribute.
   * <p>
   * E.g.: <code>removeProtocols("a", "href", "ftp")</code>
   * </p>
   *
   * @param tag       Tag the URL protocol is for
   * @param key       Attribute key
   * @param protocols List of invalid protocols
   * @return this, for chaining
   */
  def removeProtocols(tag: String, key: String, protocols: String*): Whitelist = {
    notEmpty(tag)
    notEmpty(key)
    notNull(protocols)
    val tagName: Whitelist.TagName = TagName.valueOf(tag)
    val attrKey: Whitelist.AttributeKey = AttributeKey.valueOf(key)
    if (this.protocols.contains(tagName)) {
      val attrMap: mutable.Map[Whitelist.AttributeKey, mutable.Set[Whitelist.Protocol]] = this.protocols(tagName)
      if (attrMap.contains(attrKey)) {
        val protSet: mutable.Set[Whitelist.Protocol] = attrMap(attrKey)
        for (protocol <- protocols) {
          notEmpty(protocol)
          val prot: Whitelist.Protocol = Protocol.valueOf(protocol)
          protSet.remove(prot)
        }
        if (protSet.isEmpty) {
          attrMap.remove(attrKey)
          if (attrMap.isEmpty) {
            this.protocols.remove(tagName)
          }
        }
      }
    }
    this
  }

  /**
   * Test if the supplied tag is allowed by this whitelist
   * @param tag test tag
   * @return true if allowed
   */
  protected[safety] def isSafeTag(tag: String): Boolean = {
    tagNames.contains(TagName.valueOf(tag))
  }

  /**
   * Test if the supplied attribute is allowed by this whitelist for this tag
   * @param tagName tag to consider allowing the attribute in
   * @param el element under test, to confirm protocol
   * @param attr attribute under test
   * @return true if allowed
   */
  protected[safety] def isSafeAttribute(tagName: String, el: Element, attr: Attribute): Boolean = {
    val tag: Whitelist.TagName = TagName.valueOf(tagName)
    val key: Whitelist.AttributeKey = AttributeKey.valueOf(attr.getKey)
    if (attributes.contains(tag)) {
      if (attributes(tag).contains(key)) {
        if (protocols.contains(tag)) {
          val attrProts = protocols(tag)
          return !attrProts.contains(key) || testValidProtocol(el, attr, attrProts(key))
        } else {
          return true
        }
      }
    }
    !(tagName == ":all") && isSafeAttribute(":all", el, attr)
  }

  private def testValidProtocol(el: Element, attr: Attribute, protocols: mutable.Set[Whitelist.Protocol]): Boolean = {
    var value: String = el.absUrl(attr.getKey)
    if (value.length == 0) {
      value = attr.getValue
    }
    if (!preserveRelativeLinks) {
      attr.setValue(value)
    }
    import scala.util.control.Breaks._
    for (protocol <- protocols) {
      breakable {
        var prot: String = protocol.toString
        if (prot == "#") {
          if (isValidAnchor(value)) {
            return true
          } else {
            break()
          }
        }
        prot += ":"
        if (value.toLowerCase.startsWith(prot)) {
          return true
        }
      }
    }
    false
  }

  private def isValidAnchor(value: String): Boolean = {
    value.startsWith("#") && !value.matches(".*\\s.*")
  }

  private[safety] def getEnforcedAttributes(tagName: String): Attributes = {
    val attrs: Attributes = new Attributes
    val tag: Whitelist.TagName = TagName.valueOf(tagName)
    if (enforcedAttributes.contains(tag)) {
      val keyVals: mutable.Map[Whitelist.AttributeKey, Whitelist.AttributeValue] = enforcedAttributes(tag)
      import scala.collection.JavaConversions._
      for (entry <- keyVals.entrySet) {
        attrs.put(entry.getKey.toString, entry.getValue.toString)
      }
    }
    attrs
  }
}

object Whitelist {

  /**
   * This whitelist allows only text nodes: all HTML will be stripped.
   *
   * @return whitelist
   */
  def none: Whitelist = {
    new Whitelist
  }

  /**
   * This whitelist allows only simple text formatting: <code>b, em, i, strong, u</code>. All other HTML (tags and
   * attributes) will be removed.
   *
   * @return whitelist
   */
  def simpleText: Whitelist = {
    new Whitelist().addTags("b", "em", "i", "strong", "u")
  }

  /**
   * <p>
   * This whitelist allows a fuller range of text nodes: <code>a, b, blockquote, br, cite, code, dd, dl, dt, em, i, li,
   * ol, p, pre, q, small, span, strike, strong, sub, sup, u, ul</code>, and appropriate attributes.
   * </p>
   * <p>
   * Links (<code>a</code> elements) can point to <code>http, https, ftp, mailto</code>, and have an enforced
   * <code>rel=nofollow</code> attribute.
   * </p>
   * <p>
   * Does not allow images.
   * </p>
   *
   * @return whitelist
   */
  def basic: Whitelist = {
    new Whitelist().
            addTags("a", "b", "blockquote", "br", "cite", "code", "dd", "dl", "dt", "em", "i", "li", "ol", "p", "pre", "q", "small", "span", "strike", "strong", "sub", "sup", "u", "ul")
            .addAttributes("a", "href")
            .addAttributes("blockquote", "cite")
            .addAttributes("q", "cite")
            .addProtocols("a", "href", "ftp", "http", "https", "mailto")
            .addProtocols("blockquote", "cite", "http", "https")
            .addProtocols("cite", "cite", "http", "https")
            .addEnforcedAttribute("a", "rel", "nofollow")
  }

  /**
   * This whitelist allows the same text tags as {@link #basic}, and also allows <code>img</code> tags, with appropriate
   * attributes, with <code>src</code> pointing to <code>http</code> or <code>https</code>.
   *
   * @return whitelist
   */
  def basicWithImages: Whitelist = {
    basic
            .addTags("img")
            .addAttributes("img", "align", "alt", "height", "src", "title", "width")
            .addProtocols("img", "src", "http", "https")
  }

  /**
   * This whitelist allows a full range of text and structural body HTML: <code>a, b, blockquote, br, caption, cite,
   * code, col, colgroup, dd, div, dl, dt, em, h1, h2, h3, h4, h5, h6, i, img, li, ol, p, pre, q, small, span, strike, strong, sub,
   * sup, table, tbody, td, tfoot, th, thead, tr, u, ul</code>
   * <p>
   * Links do not have an enforced <code>rel=nofollow</code> attribute, but you can add that if desired.
   * </p>
   *
   * @return whitelist
   */
  def relaxed: Whitelist = {
    new Whitelist()
            .addTags("a", "b", "blockquote", "br", "caption", "cite", "code", "col", "colgroup", "dd", "div", "dl", "dt", "em", "h1", "h2", "h3", "h4", "h5", "h6", "i", "img", "li", "ol", "p", "pre", "q", "small", "span", "strike", "strong", "sub", "sup", "table", "tbody", "td", "tfoot", "th", "thead", "tr", "u", "ul")
            .addAttributes("a", "href", "title")
            .addAttributes("blockquote", "cite")
            .addAttributes("col", "span", "width")
            .addAttributes("colgroup", "span", "width")
            .addAttributes("img", "align", "alt", "height", "src", "title", "width")
            .addAttributes("ol", "start", "type")
            .addAttributes("q", "cite")
            .addAttributes("table", "summary", "width")
            .addAttributes("td", "abbr", "axis", "colspan", "rowspan", "width")
            .addAttributes("th", "abbr", "axis", "colspan", "rowspan", "scope", "width")
            .addAttributes("ul", "type")
            .addProtocols("a", "href", "ftp", "http", "https", "mailto")
            .addProtocols("blockquote", "cite", "http", "https")
            .addProtocols("cite", "cite", "http", "https")
            .addProtocols("img", "src", "http", "https")
            .addProtocols("q", "cite", "http", "https")
  }

  // named types for config. All just hold strings, but here for my sanity.
  private[safety] class TagName private[safety](private val value: String) extends TypedValue(value)

  private[safety] object TagName {

    private[safety] def valueOf(value: String): Whitelist.TagName = {
      new Whitelist.TagName(value)
    }
  }

  private[safety] class AttributeKey private[safety](private val value: String) extends TypedValue(value)

  private[safety] object AttributeKey {

    private[safety] def valueOf(value: String): Whitelist.AttributeKey = {
      new Whitelist.AttributeKey(value)
    }
  }

  private[safety] object AttributeValue {

    private[safety] def valueOf(value: String): Whitelist.AttributeValue = {
      new Whitelist.AttributeValue(value)
    }
  }

  private[safety] class AttributeValue private[safety](private val value: String) extends TypedValue(value)

  private[safety] object Protocol {

    private[safety] def valueOf(value: String): Whitelist.Protocol = {
      new Whitelist.Protocol(value)
    }
  }

  private[safety] class Protocol private[safety](private val value: String) extends TypedValue(value)

  private[safety] abstract class TypedValue private[safety](private var value: String) {

    notNull(value)

    override def hashCode: Int = {
      val prime: Int = 31
      var result: Int = 1
      result = prime * result + (if (value == null) {
        0
      } else {
        value.hashCode
      })
      result
    }

    override def equals(obj: Any): Boolean = {
      obj match {
        case other: Whitelist.TypedValue =>
          if (this eq other) {
            return true
          }
          if (other == null) {
            return false
          }
          if (value == null) {
            if (other.value != null) {
              return false
            }
          } else if (!(value == other.value)) {
            return false
          }
          true
        case _ => false
      }
    }

    override def toString: String = {
      value
    }
  }

}
