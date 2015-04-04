package com.github.ligangty.scala.jsoup

/**
 * Text utils to ease testing
 */
object TextUtil {

  def stripNewlines(text: String): String = text.replaceAll("\\n\\s*", "")
}

