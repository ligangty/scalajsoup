package com.github.ligangty.scala.jsoup.select

import com.github.ligangty.scala.jsoup.nodes.Element

/**
 * Base structural evaluator.
 */
abstract private[select] class StructuralEvaluator(evaluator: Evaluator) extends Evaluator

private[select] object StructuralEvaluator {

  private[select] class Root extends Evaluator {

    def matches(root: Element, element: Element): Boolean = {
      root eq element
    }
  }

  private[select] class Has(evaluator: Evaluator) extends StructuralEvaluator(evaluator) {

    def matches(root: Element, element: Element): Boolean = {
      element.getAllElements.exists(e => (e ne element) && evaluator.matches(root, e))
    }

    override def toString: String = {
      ":has(%s)".format(evaluator)
    }
  }

  private[select] class Not(evaluator: Evaluator) extends StructuralEvaluator(evaluator) {

    def matches(root: Element, node: Element): Boolean = {
      !evaluator.matches(root, node)
    }

    override def toString: String = {
      ":not%s".format(evaluator)
    }
  }

  private[select] class Parent(evaluator: Evaluator) extends StructuralEvaluator(evaluator) {

    def matches(root: Element, element: Element): Boolean = {
      if (root eq element) {
        return false
      }
      var parent: Element = element.parent
      while (parent ne root) {
        if (evaluator.matches(root, parent)) {
          return true
        }
        parent = parent.parent
      }
      false
    }

    override def toString: String = {
      ":parent%s".format(evaluator)
    }
  }

  private[select] class ImmediateParent(evaluator: Evaluator) extends StructuralEvaluator(evaluator) {

    def matches(root: Element, element: Element): Boolean = {
      if (root eq element) {
        return false
      }
      val parent: Element = element.parent
      parent != null && evaluator.matches(root, parent)
    }

    override def toString: String = {
      ":ImmediateParent%s".format(evaluator)
    }
  }

  private[select] class PreviousSibling(evaluator: Evaluator) extends StructuralEvaluator(evaluator) {

    def matches(root: Element, element: Element): Boolean = {
      if (root eq element) {
        return false
      }
      var prev: Element = element.previousElementSibling
      while (prev != null) {
        if (evaluator.matches(root, prev)) {
          return true
        }
        prev = prev.previousElementSibling
      }
      false
    }

    override def toString: String = {
      ":prev*%s".format(evaluator)
    }
  }

  private[select] class ImmediatePreviousSibling(evaluator: Evaluator) extends StructuralEvaluator(evaluator) {

    def matches(root: Element, element: Element): Boolean = {
      if (root eq element) {
        return false
      }
      val prev: Element = element.previousElementSibling
      prev != null && evaluator.matches(root, prev)
    }

    override def toString: String = {
      ":prev%s".format(evaluator)
    }
  }

}
