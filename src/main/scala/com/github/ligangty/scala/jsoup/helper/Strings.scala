package com.github.ligangty.scala.jsoup.helper


import java.lang

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
    if (width < paddings.length) paddings(width) else " " * width
  }

  /**
   * Tests if a code point is "whitespace" as defined in the HTML spec.
   * @param c code point to test
   * @return true if code point is whitespace, false otherwise
   */
  def isWhitespace(c: Int): Boolean =
    c == ' ' || c == '\t' || c == '\n' || c == '\f' || c == '\r'

  /**
   * Tests if a string is blank: null, emtpy, or only whitespace (" ", \r\n, \t, etc)
   * @param string string to test
   * @return if string is blank
   */
  def isBlank(string: String): Boolean =
    if (string == null) true else string.forall(isWhitespace(_))

  /**
   * Tests if a string is numeric, i.e. contains only digit characters
   * @param string string to test
   * @return true if only digit chars, false if empty or null or contains non-digit chrs
   */
  def isNumeric(string: String): Boolean =
    if (string == null) false else string.forall(Character.isDigit(_))

  /**
   * Normalise the whitespace within this string multiple spaces collapse to a single, and all whitespace characters
   * (e.g. newline, tab) convert to a simple space
   * @param string content to normalise
   * @return normalised string
   */
  def normaliseWhitespace(string: String): String = {
    val sb = new lang.StringBuilder(string.length())
    appendNormalisedWhitespace(sb, string, false)
    sb.toString()
  }

  /**
   * After normalizing the whitespace within a string, appends it to a string builder.
   * @param accum builder to append to
   * @param string string to normalize whitespace within
   * @param stripLeading set to true if you wish to remove any leading whitespace
   */
  def appendNormalisedWhitespace(accum: lang.StringBuilder, string: String, stripLeading: Boolean) {
    val len = string.length()

    var lastWasWhite = false
    var reachedNonWhite = false
    var i = 0
    var c = 0
    while (i < len) {
      c = string.codePointAt(i)
      i += Character.charCount(c)
      if (isWhitespace(c)) {
        if (!((stripLeading && !reachedNonWhite) || lastWasWhite)) {
          accum.append(' ')
          lastWasWhite = true
        }
      }
      else {
        accum.appendCodePoint(c)
        lastWasWhite = false
        reachedNonWhite = true
      }
    }
  }

  def in(needle: String, haystack: String*): Boolean = !haystack.forall(needle != _)
}
