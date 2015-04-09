package com.github.ligangty.scala.jsoup

import java.io.IOException

/**
 * Signals that a HTTP request resulted in a not OK HTTP response.
 */
class HttpStatusException private(message: String) extends IOException(message) {

  private var statusCode: Int = 0
  private var url: String = null

  def this(message: String, statusCode: Int, url: String) {
    this(message)
    this.statusCode = statusCode
    this.url = url
  }

  def getStatusCode: Int = {
    statusCode
  }

  def getUrl: String = {
    url
  }

  override def toString: String = {
    super.toString + ". Status=" + statusCode + ", URL=" + url
  }
}
