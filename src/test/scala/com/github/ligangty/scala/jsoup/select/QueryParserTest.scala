package com.github.ligangty.scala.jsoup.select

import org.scalatest.FunSuite

/**
 * Tests for the Selector Query Parser.
 */
class QueryParserTest extends FunSuite {

  test("testOrGetsCorrectPrecedence") {
    // tests that a selector "a b, c d, e f" evals to (a AND b) OR (c AND d) OR (e AND f)"
    // top level or, three child ands
    val eval: Evaluator = QueryParser.parse("a b, c d, e f")
    assert(eval.isInstanceOf[CombiningEvaluator.Or])
    val or: CombiningEvaluator.Or = eval.asInstanceOf[CombiningEvaluator.Or]
    assert(3 == or.evaluators.size)
    import scala.collection.JavaConversions._
    for (innerEval <- or.evaluators) {
      assert(innerEval.isInstanceOf[CombiningEvaluator.And])
      val and: CombiningEvaluator.And = innerEval.asInstanceOf[CombiningEvaluator.And]
      assert(2 == and.evaluators.size)
      assert(and.evaluators.get(0).isInstanceOf[Evaluator.Tag])
      assert(and.evaluators.get(1).isInstanceOf[StructuralEvaluator.Parent])
    }
  }

  test("testParsesMultiCorrectly") {
    val eval: Evaluator = QueryParser.parse(".foo > ol, ol > li + li")
    assert(eval.isInstanceOf[CombiningEvaluator.Or])
    val or: CombiningEvaluator.Or = eval.asInstanceOf[CombiningEvaluator.Or]
    assert(2 == or.evaluators.size)
    val andLeft: CombiningEvaluator.And = or.evaluators.head.asInstanceOf[CombiningEvaluator.And]
    val andRight: CombiningEvaluator.And = or.evaluators(1).asInstanceOf[CombiningEvaluator.And]
    assert("ol :ImmediateParent.foo" == andLeft.toString)
    assert(2 == andLeft.evaluators.size)
    assert("li :prevli :ImmediateParentol" == andRight.toString)
    assert(2 == andLeft.evaluators.size)
  }
}
