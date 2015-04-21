package com.github.ligangty.scala.jsoup.parser

import com.github.ligangty.scala.jsoup.helper.Validator._
import CharacterReader._
import scala.util.control.Breaks._

/**
 * CharacterReader consumes tokens off a string. To replace the old TokenQueue.
 */
final private[parser] class CharacterReader(input: String) {

  private val inputArray: Array[Char] = {
    notNull(input)
    input.toCharArray
  }
  private val length: Int = {
    notNull(input)
    inputArray.length
  }
  private var posVal: Int = 0
  private var markVal: Int = 0
  private val stringCache: Array[String] = new Array[String](512)

  private[parser] def pos: Int = posVal

  private[parser] def isEmpty: Boolean = posVal >= length

  private[parser] def current: Char = if (posVal >= length) {
    EOF
  } else {
    inputArray(posVal)
  }

  private[parser] def consume: Char = {
    val value: Char = if (posVal >= length) {
      EOF
    } else {
      inputArray(posVal)
    }
    posVal += 1
    value
  }

  private[parser] def unconsume(): Unit = {
    posVal -= 1
  }

  private[parser] def advance(): Unit = {
    posVal += 1
  }

  private[parser] def mark(): Unit = {
    markVal = posVal
  }

  private[parser] def rewindToMark(): Unit = {
    posVal = markVal
  }

  private[parser] def consumeAsString: String = {
    val consume = new String(inputArray, posVal, 1)
    posVal += 1
    consume
  }

  /**
   * Returns the number of characters between the current position and the next instance of the input char
   * @param c scan target
   * @return offset between current position and next instance of target. -1 if not found.
   */
  private[parser] def nextIndexOf(c: Char): Int = {
    for (i <- posVal to (length - 1) if c == inputArray(i)) {
      return i - pos
    }
    -1
  }

  /**
   * Returns the number of characters between the current position and the next instance of the input sequence
   *
   * @param seq scan target
   * @return offset between current position and next instance of target. -1 if not found.
   */
  private[parser] def nextIndexOf(seq: CharSequence): Int = {
    // doesn't handle scanning for surrogates
    val startChar: Char = seq.charAt(0)
    for (offset <- posVal to (length - 1)) {
      var offs = offset
      // scan to first instance of startchar:
      if (startChar != inputArray(offs)) {
        offs += 1
        while (offs < length && startChar != inputArray(offs)) {
          offs += 1
        }
        var i: Int = offs + 1
        val last: Int = i + seq.length - 1
        if (offs < length && last <= length) {
          var j = 1
          while (i < last && seq.charAt(j) == inputArray(i)) {
            i += 1
            j += 1
          }
          if (i == last) {
            return offs - posVal
          }
        }
      }
    }
    -1
  }

  private[parser] def consumeTo(c: Char): String = {
    val offset: Int = nextIndexOf(c)
    if (offset != -1) {
      val consumed: String = cacheString(posVal, offset)
      posVal += offset
      consumed
    } else {
      consumeToEnd
    }
  }

  private[parser] def consumeTo(seq: String): String = {
    val offset: Int = nextIndexOf(seq)
    if (offset != -1) {
      val consumed: String = cacheString(posVal, offset)
      posVal += offset
      consumed
    } else {
      consumeToEnd
    }
  }

  private[parser] def consumeToAny(chars: Char*): String = consumeToAny(chars.toArray)

  private[parser] def consumeToAny(chars: Array[Char]): String = {
    val start: Int = posVal
    val remaining: Int = length
    breakable {
      while (posVal < remaining) {
        for (c <- chars if inputArray(posVal) == c) {
          break()
        }
        posVal += 1
      }
    }
    if (posVal > start) {
      cacheString(start, posVal - start)
    } else {
      ""
    }
  }

  private[parser] def consumeToAnySorted(chars: Char*): String = consumeToAnySorted(chars.toArray)

  private[parser] def consumeToAnySorted(chars: Array[Char]): String = {
    val start: Int = posVal
    val remaining: Int = length
    val value: Array[Char] = inputArray

    breakable {
      while (posVal < remaining) {
        if (java.util.Arrays.binarySearch(chars.toArray, value(posVal)) >= 0) {
          break()
        }
        posVal += 1
      }
    }
    if (posVal > start) {
      cacheString(start, posVal - start)
    } else {
      ""
    }
  }

  private[parser] def consumeData: String = {
    // &, <, null
    val start: Int = posVal
    val remaining: Int = length
    val value: Array[Char] = inputArray
    breakable {
      while (posVal < remaining) {
        val c: Char = value(posVal)
        if (c == '&' || c == '<' || c == TokeniserState.nullChar) {
          break()
        } //todo: break is not supported
        posVal += 1
      }
    }
    if (posVal > start) {
      cacheString(start, posVal - start)
    } else {
      ""
    }
  }

  private[parser] def consumeTagName: String = {
    // '\t', '\n', '\r', '\f', ' ', '/', '>', nullChar
    val start: Int = posVal
    val remaining: Int = length
    val value: Array[Char] = inputArray
    breakable {
      while (posVal < remaining) {
        val c: Char = value(posVal)
        if (c == '\t' || c == '\n' || c == '\r' || c == '\f' || c == ' ' || c == '/' || c == '>' || c == TokeniserState.nullChar) {
          break()
        }
        posVal += 1
      }
    }
    if (posVal > start) {
      cacheString(start, posVal - start)
    } else {
      ""
    }
  }

  private[parser] def consumeToEnd: String = {
    val data: String = cacheString(posVal, length - posVal)
    posVal = length
    data
  }

  private[parser] def consumeLetterSequence: String = {
    val start: Int = posVal
    breakable {
      while (posVal < length) {
        val c: Char = inputArray(posVal)
        if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')) {
          posVal += 1
        } else {
          break()
        }
      }
    }
    cacheString(start, posVal - start)
  }

  private[parser] def consumeLetterThenDigitSequence: String = {
    val start: Int = posVal
    breakable {
      while (posVal < length) {
        val c: Char = inputArray(posVal)
        if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')) {
          posVal += 1
        } else {
          break()
        }
      }
    }
    breakable {
      while (!isEmpty) {
        val c: Char = inputArray(posVal)
        if (c >= '0' && c <= '9') {
          posVal += 1
        } else {
          break()
        }
      }
    }
    cacheString(start, posVal - start)
  }

  private[parser] def consumeHexSequence: String = {
    val start: Int = posVal
    breakable {
      while (posVal < length) {
        val c: Char = inputArray(posVal)
        if ((c >= '0' && c <= '9') || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f')) {
          posVal += 1
        } else {
          break()
        }
      }
    }
    cacheString(start, posVal - start)
  }

  private[parser] def consumeDigitSequence: String = {
    val start: Int = posVal
    breakable {
      while (posVal < length) {
        val c: Char = inputArray(posVal)
        if (c >= '0' && c <= '9') {
          posVal += 1
        } else {
          break()
        }
      }
    }
    cacheString(start, posVal - start)
  }

  private[parser] def matches(c: Char): Boolean = !isEmpty && inputArray(posVal) == c

  private[parser] def matches(seq: String): Boolean = {
    val scanLength: Int = seq.length
    if (scanLength > length - posVal) {
      return false
    }
    for (offset <- 0 to (scanLength - 1) if seq.charAt(offset) != inputArray(posVal + offset)) {
      return false
    }
    true
  }

  private[parser] def matchesIgnoreCase(seq: String): Boolean = {
    val scanLength: Int = seq.length
    if (scanLength > length - posVal) {
      return false
    }
    for (offset <- 0 to (scanLength - 1)) {
      val upScan = seq.charAt(offset).toUpper
      val upTarget = inputArray(posVal + offset).toUpper
      if (upScan != upTarget) {
        return false
      }
    }
    true
  }

  private[parser] def matchesAny(seq: Char*): Boolean = matchesAny(seq.toArray)

  private[parser] def matchesAny(seq: Array[Char]): Boolean = {
    if (isEmpty) {
      return false
    }
    val c: Char = inputArray(posVal)
    for (seek <- seq if seek == c) {
      return true
    }
    false
  }

  private[parser] def matchesAnySorted(seq: Array[Char]): Boolean =
    !isEmpty && java.util.Arrays.binarySearch(seq, inputArray(posVal)) >= 0

  private[parser] def matchesLetter: Boolean = {
    if (isEmpty) {
      return false
    }
    val c: Char = inputArray(posVal)
    (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
  }

  private[parser] def matchesDigit: Boolean = {
    if (isEmpty) {
      return false
    }
    val c: Char = inputArray(posVal)
    c >= '0' && c <= '9'
  }

  private[parser] def matchConsume(seq: String): Boolean = {
    if (matches(seq)) {
      posVal += seq.length
      true
    } else {
      false
    }
  }

  private[parser] def matchConsumeIgnoreCase(seq: String): Boolean = {
    if (matchesIgnoreCase(seq)) {
      posVal += seq.length
      true
    } else {
      false
    }
  }

  private[parser] def containsIgnoreCase(seq: String): Boolean = {
    // used to check presence of </title>, </style>. only finds consistent case.
    import java.util.Locale
    val loScan: String = seq.toLowerCase(Locale.ENGLISH)
    val hiScan: String = seq.toUpperCase(Locale.ENGLISH)
    (nextIndexOf(loScan) > -1) || (nextIndexOf(hiScan) > -1)
  }

  override def toString: String = new String(inputArray, posVal, length - posVal)

  /**
   * Caches short strings, as a flywheel pattern, to reduce GC load. Just for this doc, to prevent leaks.
   * <p />
   * Simplistic, and on hash collisions just falls back to creating a new string, vs a full HashMap with Entry list.
   * That saves both having to create objects as hash keys, and running through the entry list, at the expense of
   * some more duplicates.
   */
  private def cacheString(start: Int, count: Int): String = {
    val value: Array[Char] = inputArray
    val cache: Array[String] = stringCache
    // limit (no cache):
    if (count > maxCacheLen) {
      return new String(value, start, count)
    }
    // calculate hash:
    var hash: Int = 0
    var offset: Int = start
    for (i <- 0 to (count - 1)) {
      hash = 31 * hash + value(offset)
      offset += 1
    }

    // get from cache
    val index: Int = hash & cache.length - 1
    var cached: String = cache(index)
    if (cached == null) {
      // miss, add
      cached = new String(value, start, count)
      cache(index) = cached
    } else {
      // hashcode hit, check equality
      if (rangeEquals(start, count, cached)) {
        // hit
        return cached
      } else {
        // hashcode conflict
        cached = new String(value, start, count)
      }
    }
    cached
  }

  /**
   * Check if the value of the provided range equals the string.
   */
  private[parser] def rangeEquals(start: Int, count: Int, cached: String): Boolean = {
    var cnt = count
    if (cnt == cached.length) {
      val one = inputArray
      var i: Int = start
      var j: Int = 0
      while (cnt != 0) {
        cnt -= 1
        if (one(i) != cached.charAt(j)) {
          return false
        }
        i += 1
        j += 1
      }
      return true
    }
    false
  }
}

private[parser] object CharacterReader {

  private[parser] val EOF: Char = -1.toChar
  private val maxCacheLen: Int = 12
}
