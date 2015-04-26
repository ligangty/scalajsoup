package com.github.ligangty.scala.jsoup.select

import com.github.ligangty.scala.jsoup.nodes.{Node, Element}

/**
 * Collects a list of elements that match the supplied criteria.
 *
 */
object Collector {

  /**
   * Build a list of elements, by visiting root and every descendant of root, and testing it against the evaluator.
   * @param eval Evaluator to test elements against
   * @param root root of tree to descend
   * @return list of matches; empty if none
   */
  def collect(eval: Evaluator, root: Element): Elements = {
    val elements: Elements = new Elements
    val tra = new NodeTraversor(new Collector.Accumulator(root, elements, eval))
    tra.traverse(root)
    elements
  }

  private class Accumulator private[select](root: Element, elements: Elements, eval: Evaluator) extends NodeVisitor {

    def head(node: Node, depth: Int) {
      node match {
        case el: Element if eval.matches(root, el) => elements.add(el)
        case _ =>
      }
    }

    def tail(node: Node, depth: Int): Unit = {
      // void
    }
  }

}
