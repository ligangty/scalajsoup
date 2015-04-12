package com.github.ligangty.scala.jsoup.select

import com.github.ligangty.scala.jsoup.helper.Strings
import com.github.ligangty.scala.jsoup.helper.Validator._
import com.github.ligangty.scala.jsoup.parser.TokenQueue

import scala.collection.mutable
import QueryParser._

import scala.util.matching.Regex

/**
 * Parses a CSS selector into an Evaluator tree.
 */
private[select] class QueryParser private(query: String) {

  private var tq: TokenQueue = new TokenQueue(query)
  private var evals: mutable.ArrayBuffer[Evaluator] = new mutable.ArrayBuffer[Evaluator]

  /**
   * Parse the query
   * @return Evaluator
   */
  private[select] def parse: Evaluator = {
    tq.consumeWhitespace
    if (tq.matchesAnyString(combinators: _*)) {
      evals.append(new StructuralEvaluator.Root)
      combinator(tq.consume)
    }
    else {
      findElements()
    }
    while (!tq.isEmpty) {
      val seenWhite: Boolean = tq.consumeWhitespace
      if (tq.matchesAnyString(combinators: _*)) {
        combinator(tq.consume)
      }
      else if (seenWhite) {
        combinator(' ')
      }
      else {
        findElements()
      }
    }
    if (evals.size == 1) {
      return evals.head
    }
    new CombiningEvaluator.And(evals)
  }

  private def combinator(combinator: Char) {
    tq.consumeWhitespace
    val subQuery: String = consumeSubQuery
    var rootEval: Evaluator = null
    var currentEval: Evaluator = null
    val newEval: Evaluator = QueryParser.parse(subQuery)
    var replaceRightMost: Boolean = false
    if (evals.size == 1) {
      rootEval = evals(0)
      currentEval = evals(0)
      // make sure OR (,) has precedence:
      if (rootEval.isInstanceOf[CombiningEvaluator.Or] && combinator != ',') {
        currentEval = currentEval.asInstanceOf[CombiningEvaluator.Or].rightMostEvaluator.orNull
        replaceRightMost = true
      }
    } else {
      rootEval = new CombiningEvaluator.And(evals)
      currentEval = rootEval
    }
    evals.clear()
    // for most combinators: change the current eval into an AND of the current eval and the new eval
    if (combinator == '>') {
      currentEval = new CombiningEvaluator.And(newEval, new StructuralEvaluator.ImmediateParent(currentEval))
    } else if (combinator == ' ') {
      currentEval = new CombiningEvaluator.And(newEval, new StructuralEvaluator.Parent(currentEval))
    } else if (combinator == '+') {
      currentEval = new CombiningEvaluator.And(newEval, new StructuralEvaluator.ImmediatePreviousSibling(currentEval))
    } else if (combinator == '~') {
      currentEval = new CombiningEvaluator.And(newEval, new StructuralEvaluator.PreviousSibling(currentEval))
    } else if (combinator == ',') {
      currentEval match {
        case or: CombiningEvaluator.Or =>
          or.add(newEval)
          currentEval = or
        case _ =>
          val or = new CombiningEvaluator.Or
          or.add(currentEval)
          or.add(newEval)
          currentEval = or
      }
    } else {
      throw new Selector.SelectorParseException("Unknown combinator: " + combinator)
    }
    if (replaceRightMost) {
      rootEval.asInstanceOf[CombiningEvaluator.Or].replaceRightMostEvaluator(currentEval)
    }
    else {
      rootEval = currentEval
    }
    evals.append(rootEval)
  }

  private def consumeSubQuery: String = {
    val sq: StringBuilder = new StringBuilder
    import scala.util.control.Breaks._
    breakable {
      while (!tq.isEmpty) {
        if (tq.matches("(")) {
          sq.append("(").append(tq.chompBalanced('(', ')')).append(")")
        } else if (tq.matches("[")) {
          sq.append("[").append(tq.chompBalanced('[', ']')).append("]")
        } else if (tq.matchesAnyString(QueryParser.combinators:_*)) {
          break()
        } else {
          sq.append(tq.consume)
        }
      }
    }
    sq.toString()
  }

  private def findElements() {
    if (tq.matchChomp("#")) {
      byId()
    } else if (tq.matchChomp(".")) {
      byClass()
    } else if (tq.matchesWord) {
      byTag()
    } else if (tq.matches("[")) {
      byAttribute()
    } else if (tq.matchChomp("*")) {
      allElements()
    } else if (tq.matchChomp(":lt(")) {
      indexLessThan()
    } else if (tq.matchChomp(":gt(")) {
      indexGreaterThan()
    } else if (tq.matchChomp(":eq(")) {
      indexEquals()
    } else if (tq.matches(":has(")) {
      has()
    } else if (tq.matches(":contains(")) {
      contains(false)
    } else if (tq.matches(":containsOwn(")) {
      contains(true)
    } else if (tq.matches(":matches(")) {
      matches(false)
    } else if (tq.matches(":matchesOwn(")) {
      matches(true)
    } else if (tq.matches(":not(")) {
      not()
    } else if (tq.matchChomp(":nth-child(")) {
      cssNthChild(false, false)
    } else if (tq.matchChomp(":nth-last-child(")) {
      cssNthChild(true, false)
    } else if (tq.matchChomp(":nth-of-type(")) {
      cssNthChild(false, true)
    } else if (tq.matchChomp(":nth-last-of-type(")) {
      cssNthChild(true, true)
    } else if (tq.matchChomp(":first-child")) {
      evals.append(new Evaluator.IsFirstChild)
    } else if (tq.matchChomp(":last-child")) {
      evals.append(new Evaluator.IsLastChild)
    } else if (tq.matchChomp(":first-of-type")) {
      evals.append(new Evaluator.IsFirstOfType)
    } else if (tq.matchChomp(":last-of-type")) {
      evals.append(new Evaluator.IsLastOfType)
    } else if (tq.matchChomp(":only-child")) {
      evals.append(new Evaluator.IsOnlyChild)
    } else if (tq.matchChomp(":only-of-type")) {
      evals.append(new Evaluator.IsOnlyOfType)
    } else if (tq.matchChomp(":empty")) {
      evals.append(new Evaluator.IsEmpty)
    } else if (tq.matchChomp(":root")) {
      evals.append(new Evaluator.IsRoot)
    } else {
      // unhandled
      throw new Selector.SelectorParseException("Could not parse query '%s': unexpected token at '%s'", query, tq.remainder)
    }
  }

  private def byId() {
    val id: String = tq.consumeCssIdentifier
    notEmpty(id)
    evals.append(new Evaluator.Id(id))
  }

  private def byClass() {
    val className: String = tq.consumeCssIdentifier
    notEmpty(className)
    evals.append(new Evaluator.Class(className.trim.toLowerCase))
  }

  private def byTag() {
    var tagName: String = tq.consumeElementSelector
    notEmpty(tagName)
    if (tagName.contains("|")) {
      tagName = tagName.replace("|", ":")
    }
    evals.append(new Evaluator.Tag(tagName.trim.toLowerCase))
  }

  private def byAttribute() {
    val cq: TokenQueue = new TokenQueue(tq.chompBalanced('[', ']'))
    val key: String = cq.consumeToAny(QueryParser.AttributeEvals: _*)
    notEmpty(key)
    cq.consumeWhitespace
    if (cq.isEmpty) {
      if (key.startsWith("^")) {
        evals.append(new Evaluator.AttributeStarting(key.substring(1)))
      } else {
        evals.append(new Evaluator.Attribute(key))
      }
    } else {
      if (cq.matchChomp("=")) {
        evals.append(new Evaluator.AttributeWithValue(key, cq.remainder))
      } else if (cq.matchChomp("!=")) {
        evals.append(new Evaluator.AttributeWithValueNot(key, cq.remainder))
      } else if (cq.matchChomp("^=")) {
        evals.append(new Evaluator.AttributeWithValueStarting(key, cq.remainder))
      } else if (cq.matchChomp("$=")) {
        evals.append(new Evaluator.AttributeWithValueEnding(key, cq.remainder))
      } else if (cq.matchChomp("*=")) {
        evals.append(new Evaluator.AttributeWithValueContaining(key, cq.remainder))
      } else if (cq.matchChomp("~=")) {
        evals.append(new Evaluator.AttributeWithValueMatching(key, new Regex(cq.remainder)))
      } else {
        throw new Selector.SelectorParseException("Could not parse attribute query '%s': unexpected token at '%s'", query, cq.remainder)
      }
    }
  }

  private def allElements() {
    evals.append(new Evaluator.AllElements)
  }

  // pseudo selectors :lt, :gt, :eq
  private def indexLessThan() {
    evals.append(new Evaluator.IndexLessThan(consumeIndex))
  }

  private def indexGreaterThan() {
    evals.append(new Evaluator.IndexGreaterThan(consumeIndex))
  }

  private def indexEquals() {
    evals.append(new Evaluator.IndexEquals(consumeIndex))
  }

  private def cssNthChild(backwards: Boolean, ofType: Boolean) {
    val argS: String = tq.chompTo(")").trim.toLowerCase
    var a: Int = 0
    var b: Int = 0
    if ("odd" == argS) {
      a = 2
      b = 1
    } else if ("even" == argS) {
      a = 2
      b = 0
    } else if (argS.matches(NTH_AB.toString())) {
      //todo here used scala Regex way to do pattern match, make sure it is correct
      argS match {
        case NTH_AB(g1, g2, g3, g4, _*) =>
          if (g3 != null) {
            a = g1.replaceFirst("^\\+", "").toInt
          } else {
            a = 1
          }
          if (g4 != null) {
            b = g4.replaceFirst("^\\+", "").toInt
          } else {
            b = 0
          }

      }
    } else if (argS.matches(NTH_B.toString())) {
      a = 0
      b = argS.replaceFirst("^\\+", "").toInt
    } else {
      throw new Selector.SelectorParseException("Could not parse nth-index '%s': unexpected format", argS)
    }
    if (ofType) {
      if (backwards) {
        evals.append(new Evaluator.IsNthLastOfType(a, b))
      } else {
        evals.append(new Evaluator.IsNthOfType(a, b))
      }
    } else {
      if (backwards) {
        evals.append(new Evaluator.IsNthLastChild(a, b))
      } else {
        evals.append(new Evaluator.IsNthChild(a, b))
      }
    }
  }

  private def consumeIndex: Int = {
    val indexS: String = tq.chompTo(")").trim
    isTrue(Strings.isNumeric(indexS), "Index must be numeric")
    indexS.toInt
  }

  // pseudo selector :has(el)
  private def has() {
    tq.consume(":has")
    val subQuery: String = tq.chompBalanced('(', ')')
    notEmpty(subQuery, ":has(el) subselect must not be empty")
    evals.append(new StructuralEvaluator.Has(QueryParser.parse(subQuery)))
  }

  // pseudo selector :contains(text), containsOwn(text)
  private def contains(own: Boolean) {
    tq.consume(if (own) {
      ":containsOwn"
    } else {
      ":contains"
    })
    val searchText: String = TokenQueue.unescape(tq.chompBalanced('(', ')'))
    notEmpty(searchText, ":contains(text) query must not be empty")
    if (own) {
      evals.append(new Evaluator.ContainsOwnText(searchText))
    }
    else {
      evals.append(new Evaluator.ContainsText(searchText))
    }
  }

  // :matches(regex), matchesOwn(regex)
  private def matches(own: Boolean) {
    tq.consume(if (own) {
      ":matchesOwn"
    } else {
      ":matches"
    })
    val regex: String = tq.chompBalanced('(', ')')
    notEmpty(regex, ":matches(regex) query must not be empty")
    if (own) {
      evals.append(new Evaluator.MatchesOwn(new Regex(regex)))
    } else {
      evals.append(new Evaluator.Matches(new Regex(regex)))
    }
  }

  // :not(selector)
  private def not() {
    tq.consume(":not")
    val subQuery: String = tq.chompBalanced('(', ')')
    notEmpty(subQuery, ":not(selector) subselect must not be empty")
    evals.append(new StructuralEvaluator.Not(QueryParser.parse(subQuery)))
  }
}

private[select] object QueryParser {

  private val combinators: Array[String] = Array(",", ">", "+", "~", " ")
  private val AttributeEvals: Array[String] = Array[String]("=", "!=", "^=", "$=", "*=", "~=")
  //pseudo selectors :first-child, :last-child, :nth-child, ...
  private val NTH_AB: Regex = new Regex("(?i)((\\+|-)?(\\d+)?)n(\\s*(\\+|-)?\\s*\\d+)?")
  private val NTH_B: Regex = new Regex("(\\+|-)?(\\d+)")

  /**
   * Parse a CSS query into an Evaluator.
   * @param query CSS query
   * @return Evaluator
   */
  def parse(query: String): Evaluator = {
    val p: QueryParser = new QueryParser(query)
    p.parse
  }
}
