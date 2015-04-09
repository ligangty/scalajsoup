package com.github.ligangty.scala.jsoup.helper

import org.scalatest.FunSuite
import Validator._

/**
 * tests for Validator
 */
class ValidatorTest extends FunSuite {

  test("notNull") {
    intercept[IllegalArgumentException] {
      notNull(null)
    }
    notNull("")
    intercept[IllegalArgumentException] {
      notNull(null, "must not null")
    }
    notNull("", "must not null")
  }

  test("isTrue"){
    intercept[IllegalArgumentException] {
      isTrue(1==2)
    }
    isTrue(2==2)
    intercept[IllegalArgumentException] {
      isTrue(1==2, "wrong!")
    }
    isTrue(2==2, "right!")
  }

  test("isFalse"){
    intercept[IllegalArgumentException] {
      isFalse(1==1)
    }
    isFalse(1==2)
    intercept[IllegalArgumentException] {
      isFalse(1==1, "wrong!")
    }
    isFalse(1==2, "right!")
  }

  test("notEmpty"){
    intercept[IllegalArgumentException]{
      notEmpty("")
    }
    intercept[IllegalArgumentException]{
      notEmpty(null)
    }
    intercept[IllegalArgumentException]{
      notEmpty("","should be empty")
    }
    intercept[IllegalArgumentException]{
      notEmpty(null,"should be null")
    }
    notEmpty("123")
    notEmpty("123","should not be empty")
  }

  test("fail"){
    intercept[IllegalArgumentException]{
      Validator.fail("should fail")
    }
  }
}
