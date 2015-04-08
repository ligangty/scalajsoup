package com.github.ligangty.scala.jsoup.helper

import java.io._
import java.nio.ByteBuffer
import java.nio.charset.{IllegalCharsetNameException, Charset}
import java.util.{Random, Locale}
import java.util.regex.Pattern

import com.github.ligangty.scala.jsoup.nodes.{Element, Document}
import com.github.ligangty.scala.jsoup.parser.Parser

import scala.util.matching.Regex

/**
 * Internal static utilities for handling data.
 */
object DataUtil {

  private val charsetPattern: Regex = """(?i)\bcharset=\s*(?:"|')?([^\s,;"']*)""".r
  private[helper] val defaultCharset: String = "UTF-8"
  private val bufferSize: Int = 0x20000
  private val UNICODE_BOM: Int = 0xFEFF
  private val mimeBoundaryChars: Array[Char] = "-_1234567890abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ".toCharArray
  private[helper] val boundaryLength: Int = 32

  /**
   * Loads a file to a Document.
   * @param in file to load
   * @param charsetName character set of input
   * @param baseUri base URI of document, to resolve relative links against
   * @return Document
   * @throws IOException on IO error
   */
  @throws(classOf[IOException])
  def load(in: File, charsetName: String, baseUri: String): Document = {
    val byteData: ByteBuffer = readFileToByteBuffer(in)
    parseByteData(byteData, charsetName, baseUri, Parser.htmlParser)
  }

  /**
   * Parses a Document from an input steam.
   * @param in input stream to parse. You will need to close it.
   * @param charsetName character set of input
   * @param baseUri base URI of document, to resolve relative links against
   * @return Document
   * @throws IOException on IO error
   */
  @throws(classOf[IOException])
  def load(in: InputStream, charsetName: String, baseUri: String): Document = {
    val byteData: ByteBuffer = readToByteBuffer(in)
    parseByteData(byteData, charsetName, baseUri, Parser.htmlParser)
  }

  /**
   * Parses a Document from an input steam, using the provided Parser.
   * @param in input stream to parse. You will need to close it.
   * @param charsetName character set of input
   * @param baseUri base URI of document, to resolve relative links against
   * @param parser alternate { @link Parser#xmlParser() parser} to use.
   * @return Document
   * @throws IOException on IO error
   */
  @throws(classOf[IOException])
  def load(in: InputStream, charsetName: String, baseUri: String, parser: Parser): Document = {
    val byteData: ByteBuffer = readToByteBuffer(in)
    parseByteData(byteData, charsetName, baseUri, parser)
  }

  /**
   * Writes the input stream to the output stream. Doesn't close them.
   * @param in input stream to read from
   * @param out output stream to write to
   * @throws IOException on IO error
   */
  @throws(classOf[IOException])
  private[helper] def crossStreams(in: InputStream, out: OutputStream) {
    val buffer: Array[Byte] = new Array[Byte](bufferSize)
    var len: Int = 0
    while ( {
      len = in.read(buffer)
      len
    } != -1) {
      out.write(buffer, 0, len)
    }
  }

  // reads bytes first into a buffer, then decodes with the appropriate charset. done this way to support
  // switching the chartset midstream when a meta http-equiv tag defines the charset.
  // todo - this is getting gnarly. needs a rewrite.
  private[helper] def parseByteData(byteData: ByteBuffer, charsetName: String, baseUri: String, parser: Parser): Document = {
    var docData: String = null
    var doc: Document = null
    var charsetNm = charsetName
    if (charsetNm == null) {
      // determine from meta. safe parse as UTF-8
      // look for <meta http-equiv="Content-Type" content="text/html;charset=gb2312"> or HTML5 <meta charset="gb2312">
      docData = Charset.forName(defaultCharset).decode(byteData).toString
      doc = parser.parseInput(docData, baseUri)
      val meta: Element = doc.select("meta[http-equiv=content-type], meta[charset]").first()
      if (meta != null) {
        // if not found, will keep utf-8 as best attempt
        var foundCharset: String = null
        if (meta.hasAttr("http-equiv")) {
          foundCharset = getCharsetFromContentType(meta.attr("content"))
          if (foundCharset == null && meta.hasAttr("charset")) {
            try {
              if (Charset.isSupported(meta.attr("charset"))) {
                foundCharset = meta.attr("charset")
              }
            } catch {
              case e: IllegalCharsetNameException =>
                foundCharset = null
            }
          }
        } else {
          foundCharset = meta.attr("charset")
        }
        if (foundCharset != null && foundCharset.length != 0 && foundCharset != defaultCharset) {
          foundCharset = foundCharset.trim.replaceAll("[\"']", "")
          charsetNm = foundCharset
          byteData.rewind
          docData = Charset.forName(foundCharset).decode(byteData).toString
          doc = null
        }
      }
    } else {
      // specified by content type header (or by user on file load)
      Validator.notEmpty(charsetNm, "Must set charset arg to character set of file to parse. Set to null to attempt to detect from HTML")
      docData = Charset.forName(charsetNm).decode(byteData).toString
    }
    // UTF-8 BOM indicator. takes precedence over everything else. rarely used. re-decodes incase above decoded incorrectly
    if (docData.length > 0 && docData.charAt(0) == UNICODE_BOM) {
      byteData.rewind
      docData = Charset.forName(defaultCharset).decode(byteData).toString
      docData = docData.substring(1)
      charsetNm = defaultCharset
      doc = null
    }
    if (doc == null) {
      doc = parser.parseInput(docData, baseUri)
      doc.outputSettings.charset(charsetNm)
    }
    doc
  }

  /**
   * Read the input stream into a byte buffer.
   * @param inStream the input stream to read from
   * @param maxSize the maximum size in bytes to read from the stream. Set to 0 to be unlimited.
   * @return the filled byte buffer
   * @throws IOException if an exception occurs whilst reading from the input stream.
   */
  @throws(classOf[IOException])
  private[helper] def readToByteBuffer(inStream: InputStream, maxSize: Int): ByteBuffer = {
    Validator.isTrue(maxSize >= 0, "maxSize must be 0 (unlimited) or larger")
    val capped: Boolean = maxSize > 0
    val buffer: Array[Byte] = new Array[Byte](bufferSize)
    val outStream: ByteArrayOutputStream = new ByteArrayOutputStream(bufferSize)
    var read: Int = 0
    var remaining: Int = maxSize
    import scala.util.control.Breaks._
    breakable {
      while (true) {
        read = inStream.read(buffer)
        if (read == -1) {
          break()
        }
        if (capped) {
          if (read > remaining) {
            outStream.write(buffer, 0, remaining)
            break()
          }
          remaining -= read
        }
        outStream.write(buffer, 0, read)
      }
    }
    ByteBuffer.wrap(outStream.toByteArray)
  }

  @throws(classOf[IOException])
  private[helper] def readToByteBuffer(inStream: InputStream): ByteBuffer = {
    readToByteBuffer(inStream, 0)
  }

  @throws(classOf[IOException])
  private[helper] def readFileToByteBuffer(file: File): ByteBuffer = {
    var randomAccessFile: RandomAccessFile = null
    try {
      randomAccessFile = new RandomAccessFile(file, "r")
      val bytes: Array[Byte] = new Array[Byte](randomAccessFile.length.toInt)
      randomAccessFile.readFully(bytes)
      ByteBuffer.wrap(bytes)
    } finally {
      if (randomAccessFile != null) {
        randomAccessFile.close()
      }
    }
  }

  /**
   * Parse out a charset from a content type header. If the charset is not supported, returns null (so the default
   * will kick in.)
   * @param contentType e.g. "text/html; charset=EUC-JP"
   * @return "EUC-JP", or null if not found. Charset is trimmed and uppercased.
   */
  private[helper] def getCharsetFromContentType(contentType: String): String = {
    if (contentType == null) {
      return null
    }
    val found = charsetPattern.findFirstMatchIn(contentType)
    found match {
      case Some(matched) =>
        var charset = matched.group(1).trim.replace("charset=", "")
        if (charset.length == 0) {
          return null
        }
        try {
          if (Charset.isSupported(charset)) {
            return charset
          }
          charset = charset.toUpperCase(Locale.ENGLISH)
          if (Charset.isSupported(charset)) {
            return charset
          }
        } catch {
          case e: IllegalCharsetNameException =>
            return null
        }
      case None => return null
    }
    null
  }

  /**
   * Creates a random string, suitable for use as a mime boundary
   */
  private[helper] def mimeBoundary: String = {
    val mime: StringBuilder = new StringBuilder(boundaryLength)
    val rand: Random = new Random
    for (i <- 0 to (boundaryLength - 1)) {
      mime.append(mimeBoundaryChars(rand.nextInt(mimeBoundaryChars.length)))
    }
    mime.toString()
  }
}
