package com.github.ligangty.scala.jsoup.parser

/**
 * A Parse Error records an error in the input HTML that occurs in either the tokenisation or the tree building phase.
 */
class ParseError private[parser](var pos: Int, var errorMsg: String) {

  private[parser] def this(pos: Int, errorFormat: String, args: Any*) {
    this(pos, errorFormat.format(args:_*))
  }

  /**
   * Retrieve the error message.
   * @return the error message.
   */
  def getErrorMessage: String = errorMsg

  /**
   * Retrieves the offset of the error.
   * @return error offset within input
   */
  def getPosition: Int = pos

  override def toString: String = pos + ": " + errorMsg
}
