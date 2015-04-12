package com.github.ligangty.scala.jsoup.select

import com.github.ligangty.scala.jsoup.helper.Validator
import com.github.ligangty.scala.jsoup.nodes
import com.github.ligangty.scala.jsoup.nodes._

import scala.util.matching.Regex

/**
 * Evaluates that an element matches the selector.
 */
abstract class Evaluator protected() {

  /**
   * Test if the element meets the evaluator's requirements.
   *
   * @param root    Root of the matching subtree
   * @param element tested element
   */
  def matches(root: Element, element: Element): Boolean
}

object Evaluator {

  /**
   * Evaluator for tag name
   */
  final class Tag(tagName: String) extends Evaluator {

    def matches(root: Element, element: Element): Boolean = {
      element.tagName == tagName
    }

    override def toString: String = {
      "%s".format(tagName)
    }
  }

  /**
   * Evaluator for element id
   */
  final class Id(id: String) extends Evaluator {

    def matches(root: Element, element: Element): Boolean = {
      id == element.id
    }

    override def toString: String = {
      "#%s".format(id)
    }
  }

  /**
   * Evaluator for element class
   */
  final class Class(className: String) extends Evaluator {

    def matches(root: Element, element: Element): Boolean = {
      element.hasClass(className)
    }

    override def toString: String = {
      ".%s".format(className)
    }
  }

  /**
   * Evaluator for attribute name matching
   */
  final class Attribute(key: String) extends Evaluator {

    def matches(root: Element, element: Element): Boolean = {
      element.hasAttr(key)
    }

    override def toString: String = {
      "[%s]".format(key)
    }
  }

  /**
   * Evaluator for attribute name prefix matching
   */
  final class AttributeStarting(keyPrefix: String) extends Evaluator {

    def matches(root: Element, element: Element): Boolean = {
      val values: Seq[nodes.Attribute] = element.attributes.toSeq
      values.exists(_.getKey.startsWith(keyPrefix))
    }

    override def toString: String = {
      "[^%s]".format(keyPrefix)
    }
  }

  /**
   * Evaluator for attribute name/value matching
   */
  final class AttributeWithValue(key: String, value: String) extends AttributeKeyPair(key, value) {

    def matches(root: Element, element: Element): Boolean = {
      element.hasAttr(key) && value.equalsIgnoreCase(element.attr(key))
    }

    override def toString: String = {
      "[%s=%s]".format(key, value)
    }
  }

  /**
   * Evaluator for attribute name != value matching
   */
  final class AttributeWithValueNot(key: String, value: String) extends AttributeKeyPair(key, value) {

    def matches(root: Element, element: Element): Boolean = {
      !value.equalsIgnoreCase(element.attr(key))
    }

    override def toString: String = {
      "[%s=%s]".format(key, value)
    }
  }

  /**
   * Evaluator for attribute name/value matching (value prefix)
   */
  final class AttributeWithValueStarting(key: String, value: String) extends AttributeKeyPair(key, value) {

    def matches(root: Element, element: Element): Boolean = {
      element.hasAttr(key) && element.attr(key).toLowerCase.startsWith(value)
    }

    override def toString: String = {
      "[%s^=%s]".format(key, value)
    }
  }

  /**
   * Evaluator for attribute name/value matching (value ending)
   */
  final class AttributeWithValueEnding(key: String, value: String) extends AttributeKeyPair(key, value) {

    def matches(root: Element, element: Element): Boolean = {
      element.hasAttr(key) && element.attr(key).toLowerCase.endsWith(value)
    }

    override def toString: String = {
      "[%s$=%s]".format(key, value)
    }
  }

  /**
   * Evaluator for attribute name/value matching (value containing)
   */
  final class AttributeWithValueContaining(key: String, value: String) extends AttributeKeyPair(key, value) {

    def matches(root: Element, element: Element): Boolean = {
      element.hasAttr(key) && element.attr(key).toLowerCase.contains(value)
    }

    override def toString: String = {
      "[%s*=%s]".format(key, value)
    }
  }

  /**
   * Evaluator for attribute name/value matching (value regex matching)
   */
  final class AttributeWithValueMatching(key: String, pattern: Regex) extends Evaluator {

    def matches(root: Element, element: Element): Boolean = {
      element.hasAttr(key) && pattern.findFirstIn(element.attr(key)).isDefined
    }

    override def toString: String = {
      "[%s~=%s]".format(key, pattern.toString())
    }
  }

  /**
   * Abstract evaluator for attribute name/value matching
   */
  abstract class AttributeKeyPair extends Evaluator {

    private[select] var key: String = null
    private[select] var value: String = null

    def this(key: String, value: String) {
      this()
      Validator.notEmpty(key)
      Validator.notEmpty(value)
      var valueVar = value
      this.key = key.trim.toLowerCase
      if (valueVar.startsWith("\"") && valueVar.endsWith("\"")) {
        valueVar = valueVar.substring(1, valueVar.length - 1)
      }
      this.value = valueVar.trim.toLowerCase
    }
  }

  /**
   * Evaluator for any / all element matching
   */
  final class AllElements extends Evaluator {

    def matches(root: Element, element: Element): Boolean = {
      true
    }

    override def toString: String = {
      "*"
    }
  }

  /**
   * Evaluator for matching by sibling index number (e < idx)
   */
  final class IndexLessThan(index: Int) extends IndexEvaluator(index) {

    def matches(root: Element, element: Element): Boolean = {
      element.elementSiblingIndex < index
    }

    override def toString: String = {
      ":lt(%d)".format(index)
    }
  }

  /**
   * Evaluator for matching by sibling index number (e > idx)
   */
  final class IndexGreaterThan(index: Int) extends IndexEvaluator(index) {

    def matches(root: Element, element: Element): Boolean = {
      element.elementSiblingIndex > index
    }

    override def toString: String = {
      ":gt(%d)".format(index)
    }
  }

  /**
   * Evaluator for matching by sibling index number (e = idx)
   */
  final class IndexEquals(index: Int) extends IndexEvaluator(index) {

    def matches(root: Element, element: Element): Boolean = {
      element.elementSiblingIndex == index
    }

    override def toString: String = {
      ":eq(%d)".format(index)
    }
  }

  /**
   * Evaluator for matching the last sibling (css :last-child)
   */
  final class IsLastChild extends Evaluator {

    def matches(root: Element, element: Element): Boolean = {
      val p: Element = element.parent
      p != null && !p.isInstanceOf[Document] && (element.elementSiblingIndex == p.children.size - 1)
    }

    override def toString: String = {
      ":last-child"
    }
  }

  final class IsFirstOfType extends IsNthOfType(0, 1) {

    override def toString: String = {
      ":first-of-type"
    }
  }

  final class IsLastOfType extends IsNthLastOfType(0, 1) {

    override def toString: String = {
      ":last-of-type"
    }
  }

  abstract class CssNthEvaluator(a: Int, b: Int) extends Evaluator {

    def this(b: Int) {
      this(0, b)
    }

    override def matches(root: Element, element: Element): Boolean = {
      val p: Element = element.parent
      if (p == null || p.isInstanceOf[Document]) {
        return false
      }
      val pos: Int = calculatePosition(root, element)
      if (a == 0) {
        return pos == b
      }
      (pos - b) * a >= 0 && (pos - b) % a == 0
    }

    override def toString: String = {
      if (a == 0) {
        return ":%s(%d)".format(getPseudoClass, b)
      }
      if (b == 0) {
        return ":%s(%dn)".format(getPseudoClass, a)
      }
      ":%s(%dn%+d)".format(getPseudoClass, a, b)
    }

    protected def getPseudoClass: String

    protected def calculatePosition(root: Element, element: Element): Int
  }

  /**
   * css-compatible Evaluator for :eq (css :nth-child)
   *
   * @see IndexEquals
   */
  final class IsNthChild(a: Int, b: Int) extends CssNthEvaluator(a, b) {

    override protected[select] def calculatePosition(root: Element, element: Element): Int = {
      element.elementSiblingIndex + 1
    }

    override protected[select] def getPseudoClass: String = {
      "nth-child"
    }
  }

  /**
   * css pseudo class :nth-last-child)
   *
   * @see IndexEquals
   */
  final class IsNthLastChild(a: Int, b: Int) extends CssNthEvaluator(a, b) {

    override protected[select] def calculatePosition(root: Element, element: Element): Int = {
      element.parent.children.size - element.elementSiblingIndex
    }

    override protected[select] def getPseudoClass: String = {
      "nth-last-child"
    }
  }

  /**
   * css pseudo class nth-of-type
   *
   */
  class IsNthOfType(a: Int, b: Int) extends CssNthEvaluator(a, b) {

    protected[select] def calculatePosition(root: Element, element: Element): Int = {
      var pos: Int = 0
      val family: Elements = element.parent.children
      import scala.util.control.Breaks._
      breakable {
        for (i <- 0 to (family.size - 1)) {
          if (family.get(i).tag.equals(element.tag)) {
            pos += 1
          }
          if (family.get(i) == element) {
            break()
          }
        }
      }
      pos
    }

    protected[select] def getPseudoClass: String = {
      "nth-of-type"
    }
  }

  class IsNthLastOfType(a: Int, b: Int) extends CssNthEvaluator(a, b) {

    protected[select] def calculatePosition(root: Element, element: Element): Int = {
      var pos: Int = 0
      val family: Elements = element.parent.children
      for (i <- element.elementSiblingIndex to (family.size - 1)) {
        if (family.get(i).tag == element.tag) {
          pos += 1
        }
      }
      pos
    }

    protected def getPseudoClass: String = {
      "nth-last-of-type"
    }
  }

  /**
   * Evaluator for matching the first sibling (css :first-child)
   */
  final class IsFirstChild extends Evaluator {

    override def matches(root: Element, element: Element): Boolean = {
      val p: Element = element.parent
      p != null && !p.isInstanceOf[Document] && element.elementSiblingIndex == 0
    }

    override def toString: String = {
      ":first-child"
    }
  }

  /**
   * css3 pseudo-class :root
   * @see <a href="http://www.w3.org/TR/selectors/#root-pseudo">:root selector</a>
   *
   */
  final class IsRoot extends Evaluator {

    override def matches(root: Element, element: Element): Boolean = {
      val r: Element = if (root.isInstanceOf[Document]) {
        root.child(0)
      } else {
        root
      }
      element eq r
    }

    override def toString: String = {
      ":root"
    }
  }

  final class IsOnlyChild extends Evaluator {

    override def matches(root: Element, element: Element): Boolean = {
      val p: Element = element.parent
      p != null && !p.isInstanceOf[Document] && element.siblingElements.size == 0
    }

    override def toString: String = {
      ":only-child"
    }
  }

  final class IsOnlyOfType extends Evaluator {

    override def matches(root: Element, element: Element): Boolean = {
      val p: Element = element.parent
      if (p == null || p.isInstanceOf[Document]) {
        return false
      }
      var pos: Int = 0
      val family: Elements = p.children
      for (i <- 0 to (family.size - 1)) {
        if (family.get(i).tag.equals(element.tag)) {
          pos += 1
        }
      }
      pos == 1
    }

    override def toString: String = {
      ":only-of-type"
    }
  }

  final class IsEmpty extends Evaluator {

    override def matches(root: Element, element: Element): Boolean = {
      val family: Seq[Node] = element.getChildNodes
      for (i <- 0 to (family.size - 1)) {
        val n: Node = family(i)
        if (!(n.isInstanceOf[Comment] || n.isInstanceOf[XmlDeclaration] || n.isInstanceOf[DocumentType])) {
          return false
        }
      }
      true
    }

    override def toString: String = {
      ":empty"
    }
  }

  /**
   * Abstract evaluator for sibling index matching
   *
   * @author ant
   */
  abstract class IndexEvaluator(index: Int) extends Evaluator

  /**
   * Evaluator for matching Element (and its descendants) text
   */
  final class ContainsText(searchText: String) extends Evaluator {

    override def matches(root: Element, element: Element): Boolean = {
      element.text().toLowerCase.contains(searchText)
    }

    override def toString: String = {
      ":contains(%s".format(searchText)
    }
  }

  /**
   * Evaluator for matching Element's own text
   */
  final class ContainsOwnText(searchText: String) extends Evaluator {

    override def matches(root: Element, element: Element): Boolean = {
      element.ownText.toLowerCase.contains(searchText)
    }

    override def toString: String = {
      ":containsOwn(%s".format(searchText)
    }
  }

  /**
   * Evaluator for matching Element (and its descendants) text with regex
   */
  final class Matches(pattern: Regex) extends Evaluator {

    override def matches(root: Element, element: Element): Boolean = {
      pattern.findFirstIn(element.text()).isDefined
    }

    override def toString: String = {
      ":matches(%s".format(pattern)
    }
  }

  /**
   * Evaluator for matching Element's own text with regex
   */
  final class MatchesOwn(pattern: Regex) extends Evaluator {

    def matches(root: Element, element: Element): Boolean = {
      pattern.findFirstIn(element.ownText).isDefined
    }

    override def toString: String = {
      ":matchesOwn(%s".format(pattern)
    }
  }

}
