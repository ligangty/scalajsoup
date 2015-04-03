package com.github.ligangty.scala.jsoup.parser

/**
 * A Parse Error records an error in the input HTML that occurs in either the tokenisation or the tree building phase.
 */
class ParseError {
  private var pos: Int = 0
  private var errorMsg: String = null

  private[parser] def this(pos: Int, errorMsg: String) {
    this()
    this.pos = pos
    this.errorMsg = errorMsg
  }

  private[parser] def this(pos: Int, errorFormat: String, args: Any*) {
    this()
    this.errorMsg = errorFormat.format(args)
    this.pos = pos
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
