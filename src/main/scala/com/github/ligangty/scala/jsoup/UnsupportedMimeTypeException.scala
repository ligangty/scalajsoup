package com.github.ligangty.scala.jsoup

import java.io.IOException

/**
 * Signals that a HTTP response returned a mime type that is not supported.
 */
class UnsupportedMimeTypeException private(message: String) extends IOException(message) {

  private var mimeType: String = null
  private var url: String = null

  def this(message: String, mimeType: String, url: String) {
    this(message)
    this.mimeType = mimeType
    this.url = url
  }

  def getMimeType: String = {
    mimeType
  }

  def getUrl: String = {
    url
  }

  override def toString: String = {
    super.toString + ". Mimetype=" + mimeType + ", URL=" + url
  }
}
