package com.github.ligangty.scala.jsoup.parser

import com.github.ligangty.scala.jsoup.helper.{Strings, Validator}
import scala.util.control.Breaks._

/**
 * A character queue with parsing helpers.
 *
 */
class TokenQueue private() {

  private var queue: String = null
  private var pos: Int = 0

  /**
  Create a new TokenQueue.
     @param data string of data to back queue.
    */
  def this(data: String) {
    this()
    Validator.notNull(data)
    queue = data
  }

  /**
   * Is the queue empty?
   * @return true if no data left in queue.
   */
  def isEmpty: Boolean = {
    remainingLength == 0
  }

  private def remainingLength: Int = {
    queue.length - pos
  }

  /**
   * Retrieves but does not remove the first character from the queue.
   * @return First character, or 0 if empty.
   */
  def peek: Char = {
    if (isEmpty) {
      0
    } else {
      queue.charAt(pos)
    }
  }

  /**
   * Add a character to the start of the queue (will be the next character retrieved).
   * @param c character to add
   */
  def addFirst(c: Character) {
    addFirst(c.toString)
  }

  /**
   * Add a string to the start of the queue.
   * @param seq string to add.
   */
  def addFirst(seq: String) {
    // not very performant, but an edge case
    queue = seq + queue.substring(pos)
    pos = 0
  }

  /**
   * Tests if the next characters on the queue match the sequence. Case insensitive.
   * @param seq String to check queue for.
   * @return true if the next characters match.
   */
  def matches(seq: String): Boolean = {
    queue.regionMatches(true, pos, seq, 0, seq.length)
  }

  /**
   * Case sensitive match test.
   * @param seq string to case sensitively check for
   * @return true if matched, false if not
   */
  def matchesCS(seq: String): Boolean = {
    queue.startsWith(seq, pos)
  }

  /**
   * Tests if the next characters match any of the sequences. Case insensitive.
   * @param seq list of strings to case insensitively check for
   * @return true of any matched, false if none did
   */
  def matchesAnyString(seq: String*): Boolean = {
    seq.exists(s => matches(s))
  }

  def matchesAnyChar(seq: Char*): Boolean = {
    if (isEmpty) {
      return false
    }
    seq.contains(queue.charAt(pos))
  }

  def matchesStartTag: Boolean = {
    // micro opt for matching "<x"
    remainingLength >= 2 &&
            queue.charAt(pos) == '<' &&
            Character.isLetter(queue.charAt(pos + 1))
  }

  /**
   * Tests if the queue matches the sequence (as with match), and if they do, removes the matched string from the
   * queue.
   * @param seq String to search for, and if found, remove from queue.
   * @return true if found and removed, false if not found.
   */
  def matchChomp(seq: String): Boolean = {
    if (matches(seq)) {
      pos += seq.length
      true
    } else {
      false
    }
  }

  /**
  Tests if queue starts with a whitespace character.
     @return if starts with whitespace
    */
  def matchesWhitespace: Boolean = {
    !isEmpty && Strings.isWhitespace(queue.charAt(pos))
  }

  /**
   * Test if the queue matches a word character (letter or digit).
   * @return if matches a word character
   */
  def matchesWord: Boolean = {
    !isEmpty && Character.isLetterOrDigit(queue.charAt(pos))
  }

  /**
   * Drops the next character off the queue.
   */
  def advance(): Unit = {
    if (!isEmpty) {
      pos += 1
    }
  }

  /**
   * Consume one character off queue.
   * @return first character on queue.
   */
  def consume: Char = {
    val char = queue.charAt(pos)
    pos += 1
    char
  }

  /**
   * Consumes the supplied sequence of the queue. If the queue does not start with the supplied sequence, will
   * throw an illegal state exception -- but you should be running match() against that condition.
   * <p>
   * Case insensitive.
   * @param seq sequence to remove from head of queue.
   */
  def consume(seq: String) {
    if (!matches(seq)) {
      throw new IllegalStateException("Queue did not match expected sequence")
    }
    val len: Int = seq.length
    if (len > remainingLength) {
      throw new IllegalStateException("Queue not long enough to consume sequence")
    }
    pos += len
  }

  /**
   * Pulls a string off the queue, up to but exclusive of the match sequence, or to the queue running out.
   * @param seq String to end on (and not include in return, but leave on queue). <b>Case sensitive.</b>
   * @return The matched data consumed from queue.
   */
  def consumeTo(seq: String): String = {
    val offset: Int = queue.indexOf(seq, pos)
    if (offset != -1) {
      val consumed: String = queue.substring(pos, offset)
      pos += consumed.length
      consumed
    } else {
      remainder
    }
  }

  def consumeToIgnoreCase(seq: String): String = {
    val start: Int = pos
    val first: String = seq.substring(0, 1)
    val canScan: Boolean = first.toLowerCase == first.toUpperCase
    breakable {
      while (!isEmpty) {
        if (matches(seq)) {
          break()
        }
        if (canScan) {
          val skip: Int = queue.indexOf(first, pos) - pos
          if (skip == 0) {
            // this char is the skip char, but not match, so force advance of pos
            pos += 1
          } else if (skip < 0) {
            // no chance of finding, grab to end
            pos = queue.length
          } else {
            pos += skip
          }
        } else {
          pos += 1
        }
      }
    }
    queue.substring(start, pos)
  }

  /**
   * Consumes to the first sequence provided, or to the end of the queue. Leaves the terminator on the queue.
   * @param seq any number of terminators to consume to. <b>Case insensitive.</b>
   * @return consumed string
   */
  // todo: method name. not good that consumeTo cares for case, and consume to any doesn't. And the only use for this
  // is is a case sensitive time...
  def consumeToAny(seq: String*): String = {
    val start: Int = pos
    while (!isEmpty && !matchesAnyString(seq: _*)) {
      pos += 1
    }
    queue.substring(start, pos)
  }

  /**
   * Pulls a string off the queue (like consumeTo), and then pulls off the matched string (but does not return it).
   * <p>
   * If the queue runs out of characters before finding the seq, will return as much as it can (and queue will go
   * isEmpty() == true).
   * @param seq String to match up to, and not include in return, and to pull off queue. <b>Case sensitive.</b>
   * @return Data matched from queue.
   */
  def chompTo(seq: String): String = {
    val data: String = consumeTo(seq)
    matchChomp(seq)
    data
  }

  def chompToIgnoreCase(seq: String): String = {
    val data: String = consumeToIgnoreCase(seq) // case insensitive scan
    matchChomp(seq)
    data
  }

  /**
   * Pulls a balanced string off the queue. E.g. if queue is "(one (two) three) four", (,) will return "one (two) three",
   * and leave " four" on the queue. Unbalanced openers and closers can be escaped (with \). Those escapes will be left
   * in the returned string, which is suitable for regexes (where we need to preserve the escape), but unsuitable for
   * contains text strings; use unescape for that.
   * @param open opener
   * @param close closer
   * @return data matched from the queue
   */
  def chompBalanced(open: Char, close: Char): String = {
    var start: Int = -1
    var end: Int = -1
    var depth: Int = 0
    var last: Char = 0
    breakable {
      do {
        if (isEmpty) {
          break()
        }
        val c: Character = consume
        if (last == 0 || last != TokenQueue.ESC) {
          if (c == open) {
            depth += 1
            if (start == -1) {
              start = pos
            }
          } else if (c == close) {
            depth -= 1
          }
        }
        if (depth > 0 && last != 0) {
          end = pos // don't include the outer match pair in the return
        }
        last = c
      } while (depth > 0)
    }
    if (end >= 0) {
      queue.substring(start, end)
    } else {
      ""
    }
  }

  /**
   * Pulls the next run of whitespace characters of the queue.
   * @return Whether consuming whitespace or not
   */
  def consumeWhitespace: Boolean = {
    var seen: Boolean = false
    while (matchesWhitespace) {
      pos += 1
      seen = true
    }
    seen
  }

  /**
   * Retrieves the next run of word type (letter or digit) off the queue.
   * @return String of word characters from queue, or empty string if none.
   */
  def consumeWord: String = {
    val start: Int = pos
    while (matchesWord) {
      pos += 1
    }
    queue.substring(start, pos)
  }

  /**
   * Consume an tag name off the queue (word or :, _, -)
   *
   * @return tag name
   */
  def consumeTagName: String = {
    val start: Int = pos
    while (!isEmpty && (matchesWord || matchesAnyChar(':', '_', '-'))) {
      pos += 1
    }
    queue.substring(start, pos)
  }

  /**
   * Consume a CSS element selector (tag name, but | instead of : for namespaces, to not conflict with :pseudo selects).
   *
   * @return tag name
   */
  def consumeElementSelector: String = {
    val start: Int = pos
    while (!isEmpty && (matchesWord || matchesAnyChar('|', '_', '-'))) {
      pos += 1
    }
    queue.substring(start, pos)
  }

  /**
   * Consume a CSS identifier (ID or class) off the queue (letter, digit, -, _)
   * http://www.w3.org/TR/CSS2/syndata.html#value-def-identifier
   * @return identifier
   */
  def consumeCssIdentifier: String = {
    val start: Int = pos
    while (!isEmpty && (matchesWord || matchesAnyChar('-', '_'))) {
      pos += 1
    }
    queue.substring(start, pos)
  }

  /**
   * Consume an attribute key off the queue (letter, digit, -, _, :")
   * @return attribute key
   */
  def consumeAttributeKey: String = {
    val start: Int = pos
    while (!isEmpty && (matchesWord || matchesAnyChar('-', '_', ':'))) {
        pos += 1
    }
    queue.substring(start, pos)
  }

  /**
  Consume and return whatever is left on the queue.
     @return remained of queue.
    */
  def remainder: String = {
    val remainder: String = queue.substring(pos, queue.length)
    pos = queue.length
    remainder
  }

  override def toString: String = {
    queue.substring(pos)
  }
}

object TokenQueue {

  private[TokenQueue] val ESC: Char = '\\'

  /**
   * Unescaped a \ escaped string.
   * @param in backslash escaped string
   * @return unescaped string
   */
  def unescape(in: String): String = {
    val out: StringBuilder = new StringBuilder
    var last: Char = 0
    for (c <- in.toCharArray) {
      if (c == ESC) {
        if (last != 0 && last == ESC) {
          out.append(c)
        }
      } else {
        out.append(c)
      }
      last = c
    }
    out.toString()
  }
}