package com.github.ligangty.scala.jsoup.helper


import scala.collection.TraversableLike

/**
 * Created by gli on 15-3-10.
 */
final object Strings {
  private val paddings: Array[String] = Array("", " ", "  ", "   ", "    ", "     ", "      ", "       ", "        ", "         ", "          ")

  /**
   * Join a collection of objects by a seperator to string
   * @param strings collection of objects
   * @param sep string to place between objects
   * @return joined string
   */
  def join(strings: Iterable[_ <: Any], sep: String): String = strings.mkString(sep)

  /**
   * Join a Array of objects by a seperator
   * @param strings Array of objects
   * @param sep string to place between objects
   * @return joined string
   */
  def join(strings: Array[_ <: Any], sep: String): String = strings.mkString(sep)

  /**
   * Returns space padding
   * @param width amount of padding desired
   * @return string of spaces * width
   */
  def padding(width: Integer): String = {
    if (width < 0) throw new IllegalArgumentException("width must be > 0")
    if (width < paddings.length) paddings(width)
    val out: Array[Char] = new Array[Char](width)
    var i: Int = 0
    for (i <- 0 to width - 1) {
      out(i) = ' '
    }
    String.valueOf(out)
  }

}
