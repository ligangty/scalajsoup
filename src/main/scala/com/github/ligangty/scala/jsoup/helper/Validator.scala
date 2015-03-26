package com.github.ligangty.scala.jsoup.helper

/**
  */
final object Validator {

  /**
   * Validates that the object is not null
   * @param obj object to test
   */
  def notNull(obj: AnyRef) {
    if (obj == null) throw new IllegalArgumentException("Object must not be null")
  }

  /**
   * Validates that the object is not null
   * @param obj object to test
   * @param msg message to output if validation fails
   */
  def notNull(obj: AnyRef, msg: String) {
    if (obj == null) throw new IllegalArgumentException(msg)
  }

  /**
   * Validates that the value is true
   * @param value object to test
   */
  def isTrue(value: Boolean) {
    if (!value) throw new IllegalArgumentException("Must be true")
  }

  /**
   * Validates that the value is true
   * @param value object to test
   * @param msg message to output if validation fails
   */
  def isTrue(value: Boolean, msg: String) {
    if (!value) throw new IllegalArgumentException(msg)
  }

  /**
   * Validates that the value is false
   * @param value object to test
   */
  def isFalse(value: Boolean) {
    if (value) throw new IllegalArgumentException("Must be false")
  }

  /**
   * Validates that the value is false
   * @param value object to test
   * @param msg message to output if validation fails
   */
  def isFalse(value: Boolean, msg: String) {
    if (value) throw new IllegalArgumentException(msg)
  }

  /**
   * Validates that the array contains no null elements
   * @param objects the array to test
   */
  def noNullElements(objects: Array[AnyRef]) =
    noNullElements("Array must not contain any null objects", objects)


  /**
   * Validates that the array contains no null elements
   * @param objects the array to test
   */
  def noNullElements(objects: AnyRef*) =
    noNullElements("Array must not contain any null objects", objects)


  /**
   * Validates that the array contains no null elements
   * @param objects the array to test
   * @param msg message to output if validation fails
   */
  def noNullElements(msg: String, objects: Array[AnyRef]) =
    for (obj <- objects) if (obj == null) throw new IllegalArgumentException(msg)

  /**
   * Validates that the array contains no null elements
   * @param objects the array to test
   * @param msg message to output if validation fails
   */
  def noNullElements(msg: String, objects: AnyRef*) {
    for (obj <- objects) if (obj == null) throw new IllegalArgumentException(msg)
  }

  /**
   * Validates that the string is not empty
   * @param string the string to test
   */
  def notEmpty(string: String) {
    if (string == null || string.length == 0) throw new IllegalArgumentException("String must not be empty")
  }

  /**
   * Validates that the string is not empty
   * @param string the string to test
   * @param msg message to output if validation fails
   */
  def notEmpty(string: String, msg: String) {
    if (string == null || string.length == 0) throw new IllegalArgumentException(msg)
  }

  /**
  Cause a failure.
     @param msg message to output.
    */
  def fail(msg: String) {
    throw new IllegalArgumentException(msg)
  }

}
