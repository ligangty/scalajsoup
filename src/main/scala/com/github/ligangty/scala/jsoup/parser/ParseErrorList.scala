package com.github.ligangty.scala.jsoup.parser

import scala.collection.mutable

/**
 * A container for ParseErrors.
 */
private[parser] class ParseErrorList(initialSize: Int) extends mutable.ArrayBuffer[ParseError](initialSize) {

  private var maxSize: Int = 0

  private[parser] def this(initialCapacity: Int, maxSize: Int) {
    this(initialCapacity)
    this.maxSize = maxSize
  }

  private[parser] def canAddError: Boolean = size < maxSize

  private[parser] def getMaxSize: Int = maxSize
}
private[parser] object ParseErrorList{
  private val INITIAL_CAPACITY: Int = 16

  private[parser] def noTracking: ParseErrorList = new ParseErrorList(0, 0)

  private[parser] def tracking(maxSize: Int): ParseErrorList = new ParseErrorList(INITIAL_CAPACITY, maxSize)

}
