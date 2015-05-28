package com.github.ligangty.scala.jsoup.nodes

import java.nio.charset.CharsetEncoder

import com.github.ligangty.scala.jsoup.helper.Strings
import com.github.ligangty.scala.jsoup.parser.Parser

import scala.collection.mutable
import scala.collection.JavaConversions._
import scala.io.Source
import scala.util.control.Breaks._

/**
 * HTML entities, and escape routines.
 * Source: <a href="http://www.w3.org/TR/html5/named-character-references.html#named-character-references">W3C HTML
 * named character references</a>.
 */
object Entities {

  sealed trait EscapeMode {

    def getMap: Map[Char, String]

    def apply(key: Char) = getMap(key)
  }

  /** Restricted entities suitable for XHTML output: lt, gt, amp, and quot only. */
  case object XHTML extends EscapeMode {

    val getMap = xhtmlByVal
  }

  /** Default HTML output entities. */
  case object BASE extends EscapeMode {

    val getMap = baseByVal
  }

  /** Complete HTML entities. */
  case object EXTENDED extends EscapeMode {

    val getMap = fullByVal
  }

  case class UnknownMode(getMap: Map[Char, String]) extends EscapeMode

  // extended and overblown.
  private val full: Map[String, Char] = loadEntities("entities-full.properties")
  private val xhtmlByVal: Map[Char, String] = Map(0x00022.toChar -> "quot", 0x00026.toChar -> "amp", 0x0003C.toChar -> "lt", 0x0003E.toChar -> "gt")
  // most common / default
  private val base: Map[String, Char] = loadEntities("entities-base.properties")
  private val baseByVal: Map[Char, String] = toCharacterKey(base)
  private val fullByVal: Map[Char, String] = toCharacterKey(full)

  /**
   * Check if the input is a known named entity
   * @param name the possible entity name (e.g. "lt" or "amp")
   * @return true if a known named entity
   */
  def isNamedEntity(name: String) = full.contains(name)

  /**
   * Check if the input is a known named entity in the base entity set.
   * @param name the possible entity name (e.g. "lt" or "amp")
   * @return true if a known named entity in the base set
   * @see [[isNamedEntity(String)]]
   */
  def isBaseNamedEntity(name: String): Boolean = base.contains(name)

  /**
   * Get the Character value of the named entity
   * @param name named entity (e.g. "lt" or "amp")
   * @return the Character value of the named entity (e.g. '{ @literal <}' or '{ @literal &}')
   */
  def getCharacterByName(name: String): Char = {
    val result = full(name)
    result
  }

  private[nodes] def escape(string: String, out: Document.OutputSettings): String = {
    val accum: StringBuilder = new StringBuilder(string.length * 2)
    escape(accum, string, out, false, false, false)
    accum.toString()
  }

  /*
   * Provides a fast-path for Encoder.canEncode, which drastically improves performance on Android post JellyBean.
   * After KitKat, the implementation of canEncode degrades to the point of being useless. For non ASCII or UTF,
   * performance may be bad. We can add more encoders for common character sets that are impacted by performance
   * issues on Android if required.
   *
   * Benchmarks:     *
   * OLD toHtml() impl v New (fastpath) in millis
   * Wiki: 1895, 16
   * CNN: 6378, 55
   * Alterslash: 3013, 28
   * Jsoup: 167, 2
   */
  private def canEncode(charset: Entities.CoreCharset, c: Char, fallback: CharsetEncoder): Boolean = {
    charset match {
      case CoreCharset.ascii =>
        c < 0x80
      case CoreCharset.utf =>
        true
      case _ =>
        fallback.canEncode(c)
    }
  }

  private[Entities] object CoreCharset extends Enumeration {

    val ascii, utf, fallback = Value

    private[Entities] def byName(name: String): CoreCharset.Value = {
      if (name == "US-ASCII") {
        return ascii
      }
      if (name.startsWith("UTF-")) {
        return utf
      }
      fallback
    }

  }

  private[Entities] type CoreCharset = CoreCharset.Value

  // this method is ugly, and does a lot. but other breakups cause rescanning and stringbuilder generations
  private[nodes] def escape(accum: StringBuilder, string: String, out: Document.OutputSettings, inAttribute: Boolean, normaliseWhite: Boolean, stripLeadingWhite: Boolean): Unit = {
    var lastWasWhite: Boolean = false
    var reachedNonWhite: Boolean = false
    val escapeMode: Entities.EscapeMode = out.escapeMode
    val encoder: CharsetEncoder = out.encoder
    val coreCharset: CoreCharset = CoreCharset.byName(encoder.charset().name())
    val escapeModeMap: Map[Char, String] = escapeMode.getMap
    val length: Int = string.length
    var offset: Int = 0
    while (offset < length) {
      val codePoint = string.codePointAt(offset)
      breakable {
        if (normaliseWhite) {
          if (Strings.isWhitespace(codePoint)) {
            if ((stripLeadingWhite && !reachedNonWhite) || lastWasWhite) {
              break()
            }
            accum.append(' ')
            lastWasWhite = true
            break()
          } else {
            lastWasWhite = false
            reachedNonWhite = true
          }
        }
        // surrogate pairs, split implementation for efficiency on single char common case (saves creating strings, char[]):
        if (codePoint < Character.MIN_SUPPLEMENTARY_CODE_POINT) {
          val c: Char = codePoint.toChar
          // html specific and required escapes:
          c match {
            case '&' => accum.append("&amp;")
            case 0xA0 =>
              if (escapeMode == XHTML) {
                accum.append("&nbsp;")
              } else {
                accum.append(c)
              }
            case '<' =>
              if (!inAttribute) {
                accum.append("&lt;")
              } else {
                accum.append(c)
              }
            case '>' =>
              if (!inAttribute) {
                accum.append("&gt;")
              } else {
                accum.append(c)
              }
            case '"' =>
              if (inAttribute) {
                accum.append("&quot;")
              } else {
                accum.append(c)
              }
            case _ =>
              if (canEncode(coreCharset, c, encoder)) {
                accum.append(c)
              } else if (escapeModeMap.containsKey(c)) {
                accum.append('&').append(escapeModeMap(c)).append(';')
              } else {
                accum.append("&#x").append(Integer.toHexString(codePoint)).append(';')
              }
          }
        } else {
          val c: String = new String(Character.toChars(codePoint))
          if (encoder.canEncode(c)) {
            // uses fallback encoder for simplicity
            accum.append(c)
          } else {
            accum.append("&#x").append(Integer.toHexString(codePoint)).append(';')
          }
        }
      }
      offset += Character.charCount(codePoint)
    }
  }

  /**
   * Unescape the input string.
   * @param string input
   * @return
   */
  private[nodes] def unescape(string: String): String = unescape(string, false)

  /**
   * Unescape the input string.
   * @param string to un-HTML-escape
   * @param strict if "strict" (that is, requires trailing ';' char, otherwise that's optional)
   * @return unescaped string
   */
  private[nodes] def unescape(string: String, strict: Boolean): String = {
    Parser.unescapeEntities(string, strict)
  }

  private def loadEntities(filename: String): Map[String, Char] = {
    val propsToTuple = (v: String) => v.split("=") match {
      case Array(key: String, value: String) => (key, Integer.parseInt(value, 16).toChar)
    }
    Source.fromInputStream(Entities.getClass.getResourceAsStream(filename))
            .getLines()
            .map(propsToTuple)
            .toMap
  }

  private def toCharacterKey(inMap: Map[String, Char]): Map[Char, String] = {
    val outMap: mutable.Map[Char, String] = new mutable.HashMap[Char, String]
    for (entry <- inMap) {
      val character: Char = entry._2
      val name: String = entry._1
      if (outMap.contains(character)) {
        // dupe, prefer the lower case version
        if (name.toLowerCase == name) {
          outMap(character) = name
        }
      } else {
        outMap(character) = name
      }
    }
    outMap.toMap
  }
}
