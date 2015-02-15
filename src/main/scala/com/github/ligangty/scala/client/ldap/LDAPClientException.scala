package com.github.ligangty.scala.client.ldap

class LDAPClientException(message: String, cause: Throwable, enableSuppression: Boolean, writableStackTrace: Boolean) extends Exception(message, cause, enableSuppression, writableStackTrace) {

  /**
   * do not support a no-arg constructor to make exception meaningful
   */
  private def this() = this(null, null, true, true)

  def this(message: String, cause: Throwable) = this(message, cause, true, true)

  def this(message: String) = this(message, null)

  def this(cause: Throwable) = this(null, cause)


}
