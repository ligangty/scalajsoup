package com.github.ligangty.scala.jsoup.nodes

import java.io.{IOException, InputStream}
import java.util.{MissingResourceException, Properties}

import scala.collection.mutable
import scala.collection.JavaConversions._

/**
 */
object Entities {

  object EscapeMode extends Enumeration {
    val xhtml = EscapeModeVal[Char,String](xhtmlByVal)
    val base = EscapeModeVal[Char,String](baseByVal)
    val extended = EscapeModeVal[Char,String](fullByVal)

    import scala.language.implicitConversions
    protected case class EscapeModeVal[T <: Any, K <: Any](val map: Map[T, K]) {
      def apply(charVal:T) = map(charVal)
    }

    implicit def convert(value: Value) = value.asInstanceOf[EscapeModeVal[Char,String]]
  }

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

  private def loadEntities(filename: String): Map[String, Char] = {
    val properties: Properties = new Properties
    try {
      val in: InputStream = Entities.getClass.getResourceAsStream(filename)
      properties.load(in)
      in.close
    }
    catch {
      case e: IOException => {
        throw new MissingResourceException("Error loading entities resource: " + e.getMessage, "Entities", filename)
      }
    }

    properties.map({ case (key, value) => (key, Integer.parseInt(value, 16).toChar)}).toMap
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
