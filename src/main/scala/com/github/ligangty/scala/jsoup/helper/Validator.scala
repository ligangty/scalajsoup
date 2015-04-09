package com.github.ligangty.scala.jsoup.helper

/**
  */
object Validator {

  /**
   * Validates that the object is not null
   * @param obj object to test
   */
  def notNull(obj: AnyRef) {
    require(obj != null, "Object must not be null")
  }

  /**
   * Validates that the object is not null
   * @param obj object to test
   * @param msg message to output if validation fails
   */
  def notNull(obj: AnyRef, msg: String) {
    require(obj != null, msg)
  }

  /**
   * Validates that the value is true
   * @param value object to test
   */
  def isTrue(value: Boolean) {
    require(value, "Must be true")
  }

  /**
   * Validates that the value is true
   * @param value object to test
   * @param msg message to output if validation fails
   */
  def isTrue(value: Boolean, msg: String) {
    require(value, msg)
  }

  /**
   * Validates that the value is false
   * @param value object to test
   */
  def isFalse(value: Boolean): Unit = {
    require(!value, "Must be false")
  }

  /**
   * Validates that the value is false
   * @param value object to test
   * @param msg message to output if validation fails
   */
  def isFalse(value: Boolean, msg: String): Unit = {
    require(!value, msg)
  }

  /**
   * Validates that the array contains no null elements
   * @param objects the array to test
   */
  def noNullElements(objects: Array[Any]): Unit =
    noNullElements("Array must not contain any null objects", objects)

  /**
   * Validates that the array contains no null elements
   * @param objects the array to test
   */
  def noNullElements(objects: Any*): Unit =
    noNullElements("Array must not contain any null objects", objects)

  /**
   * Validates that the array contains no null elements
   * @param objects the array to test
   * @param msg message to output if validation fails
   */
  def noNullElements(msg: String, objects: Array[AnyRef]): Unit =
    for (obj <- objects) {
      if (obj == null) {
        throw new IllegalArgumentException(msg)
      }
    }

  /**
   * Validates that the array contains no null elements
   * @param objects the array to test
   * @param msg message to output if validation fails
   */
  def noNullElements(msg: String, objects: Any*): Unit =
    for (obj <- objects) {
      if (obj == null) {
        throw new IllegalArgumentException(msg)
      }
    }

  /**
   * Validates that the string is not empty
   * @param string the string to test
   */
  def notEmpty(string: String) {
    notEmpty(string,"String must not be empty")
  }

  /**
   * Validates that the string is not empty
   * @param string the string to test
   * @param msg message to output if validation fails
   */
  def notEmpty(string: String, msg: String) {
    require(string != null && string.length > 0, msg)
  }

  /**
  Cause a failure.
     @param msg message to output.
    */
  def fail(msg: String) {
    throw new IllegalArgumentException(msg)
  }

}
