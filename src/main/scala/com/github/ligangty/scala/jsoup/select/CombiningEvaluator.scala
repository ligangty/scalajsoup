package com.github.ligangty.scala.jsoup.select

import com.github.ligangty.scala.jsoup.helper.Strings
import com.github.ligangty.scala.jsoup.nodes.Element

import scala.collection.mutable

/**
 * Base combining (and, or) evaluator.
 */
abstract private[select] class CombiningEvaluator private[select]() extends Evaluator {

  private[select] val evaluators: mutable.ArrayBuffer[Evaluator] = new mutable.ArrayBuffer[Evaluator]
  private[select] var num: Int = 0

  private[select] def this(evals: Traversable[Evaluator]) {
    this()
    this.evaluators ++= evals
    updateNumEvaluators()
  }

  private[select] def rightMostEvaluator: Option[Evaluator] = {
    if (num > 0) {
      Some(evaluators(num - 1))
    } else {
      None
    }
  }

  private[select] def replaceRightMostEvaluator(replacement: Evaluator) {
    evaluators.update(num - 1, replacement)
  }

  private[select] def updateNumEvaluators(): Unit = {
    // used so we don't need to bash on size() for every match test
    num = evaluators.size
  }
}

private[select] object CombiningEvaluator {

  private[select] final class And private[select](evals: Traversable[Evaluator]) extends CombiningEvaluator(evals) {

    private[select] def this(evaluators: Evaluator*) {
      this(evaluators.toSeq)
    }

    override def matches(root: Element, node: Element): Boolean = {
      for (i <- 0 to (num - 1)) {
        val s: Evaluator = evaluators(i)
        if (!s.matches(root, node)) {
          return false
        }
      }
      true
    }

    override def toString: String = {
      Strings.join(evaluators, " ")
    }
  }

  private[select] final class Or private[select]() extends CombiningEvaluator {

    /**
     * Create a new Or evaluator. The initial evaluators are ANDed together and used as the first clause of the OR.
     * @param evals initial OR clause (these are wrapped into an AND evaluator).
     */
    private[select] def this(evals: Traversable[Evaluator]) {
      this()
      if (num > 1) {
        this.evaluators.append(new CombiningEvaluator.And(evals))
      }
      else {
        this.evaluators ++= evals
      }
      updateNumEvaluators()
    }

    def add(e: Evaluator) {
      evaluators.append(e)
      updateNumEvaluators()
    }

    override def matches(root: Element, node: Element): Boolean = {
      for (i <- 0 to (num - 1)) {
        val s: Evaluator = evaluators(i)
        if (s.matches(root, node)) {
          return true
        }
      }
      false
    }

    override def toString: String = {
      ":or%s".format(evaluators)
    }
  }

}