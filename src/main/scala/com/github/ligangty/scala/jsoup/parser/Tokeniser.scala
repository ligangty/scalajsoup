package com.github.ligangty.scala.jsoup.parser

/**
 */
final class Tokeniser {
  private[parser] val replacementChar: Char = '\uFFFD'
  private val notCharRefCharsSorted: Array[Char] = Array[Char]('\t', '\n', '\r', '\f', ' ', '<', '&').sorted
}
