package com.github.ligangty.scala.jsoup.nodes

import java.nio.charset.CharsetEncoder

import com.github.ligangty.scala.jsoup.helper.Strings

import scala.collection.mutable
import scala.collection.JavaConversions._
import scala.io.Source
import scala.util.control.Breaks._

/**
  */
object Entities {

  sealed trait EscapeMode {
    def getMap: Map[Char, String]

    def apply(key: Char) = getMap(key)
  }

  case object XHTML extends EscapeMode {
    val getMap = xhtmlByVal
  }

  case object BASE extends EscapeMode {
    val getMap = baseByVal
  }

  case object EXTENDED extends EscapeMode {
    val getMap = fullByVal
  }

  case class UnknownMode(getMap: Map[Char, String]) extends EscapeMode

  //  object EscapeMode extends Enumeration {
  //    val xhtml = EscapeModeVal[Char,String](xhtmlByVal)
  //    val base = EscapeModeVal[Char,String](baseByVal)
  //    val extended = EscapeModeVal[Char,String](fullByVal)
  //
  //    import scala.language.implicitConversions
  //    private[EscapeMode] case class EscapeModeVal[Char, String](val map: Map[Char, String]) {
  //      def apply(charVal:Char) = map(charVal)
  //    }
  //
  //    implicit def convert(value: Value) = value.asInstanceOf[EscapeModeVal[Char,String]]
  ////    implicit def convert(escModeVal: EscapeModeVal[Char,String]) = escModeVal.asInstanceOf[Value]
  //  }


  private val full: Map[String, Char] = loadEntities("entities-full.properties")
  lazy private val xhtmlByVal: Map[Char, String] = Map(0x00022.toChar -> "quot", 0x00026.toChar -> "amp", 0x0003C.toChar -> "lt", 0x0003E.toChar -> "gt")
  private val base: Map[String, Char] = loadEntities("entities-base.properties")
  lazy private val baseByVal: Map[Char, String] = toCharacterKey(base)
  lazy private val fullByVal: Map[Char, String] = toCharacterKey(full)

  def isNamedEntity(name: String) = full.contains(name)

  def isBaseNamedEntity(name: String): Boolean = base.contains(name)

  def getCharacterByName(name: String): Char = {
    val result = full(name)
    result
  }

  private[nodes] def escape(string: String, out: Document.OutputSettings): String = {
    val accum: StringBuilder = new StringBuilder(string.length * 2)
    escape(accum, string, out, false, false, false)
    accum.toString()
  }

  private[nodes] def escape(accum: StringBuilder, string: String, out: Document.OutputSettings, inAttribute: Boolean, normaliseWhite: Boolean, stripLeadingWhite: Boolean): Unit = {
    var lastWasWhite: Boolean = false
    var reachedNonWhite: Boolean = false
    val escapeMode: Entities.EscapeMode = out.escapeMode
    val encoder: CharsetEncoder = out.encoder
    val escapeModeMap: Map[Char, String] = escapeMode.getMap
    val length: Int = string.length
    var offset: Int = 0
    while (offset < length) {
      val codePoint = string.codePointAt(offset)
      breakable {
        if (normaliseWhite) {
          if (Strings.isWhitespace(codePoint)) {
            if ((stripLeadingWhite && !reachedNonWhite) || lastWasWhite) break()
            accum.append(' ')
            lastWasWhite = true
            break()
          }
          else {
            lastWasWhite = false
            reachedNonWhite = true
          }
        }
        if (codePoint < Character.MIN_SUPPLEMENTARY_CODE_POINT) {
          val c: Char = codePoint.toChar
          c match {
            case '&' => accum.append("&amp;")
            case 0xA0 =>
              if (escapeMode == XHTML) accum.append("&nbsp;")
              else accum.append(c)
            case '<' =>
              if (!inAttribute) accum.append("&lt;")
              else accum.append(c)
            case '>' =>
              if (!inAttribute) accum.append("&gt;")
              else accum.append(c)
            case '"' =>
              if (inAttribute) accum.append("&quot;")
              else accum.append(c)
            case _ =>
              if (encoder.canEncode(c)) accum.append(c)
              else if (escapeModeMap.containsKey(c)) accum.append('&').append(escapeModeMap(c)).append(';')
              else accum.append("&#x").append(Integer.toHexString(codePoint)).append(';')
          }
        }
        else {
          val c: String = new String(Character.toChars(codePoint))
          if (encoder.canEncode(c)) accum.append(c)
          else accum.append("&#x").append(Integer.toHexString(codePoint)).append(';')
        }
      }
      offset += Character.charCount(codePoint)
    }
  }

  private[nodes] def unescape(string: String): String = unescape(string, false)

  /**
   * Unescape the input string.
   * @param string string
   * @param strict if "strict" (that is, requires trailing ';' char, otherwise that's optional)
   * @return
   */
  private[nodes] def unescape(string: String, strict: Boolean): String = {
//    return Parser.unescapeEntities(string, strict)
    ""
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
    for (entry <- inMap.entrySet) {
      val character: Character = entry.getValue
      val name: String = entry.getKey
      if (outMap.containsKey(character)) {
        if (name.toLowerCase == name) outMap(character) = name
      }
      else {
        outMap(character) = name
      }
    }
    outMap.toMap
  }
}
