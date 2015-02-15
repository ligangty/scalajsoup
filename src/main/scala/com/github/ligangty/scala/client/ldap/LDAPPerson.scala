package com.github.ligangty.scala.client.ldap

import org.apache.commons.lang3.builder.ToStringBuilder

class LDAPPerson {

  var userName: String = _
  var realName: String = _
  var email: String = _
  var managerUsername: String = _
  var costCenter: String = _
  var department: String = _
  var jobTitle: String = _
  var employeeNumber: String = _
  var employeeType: String = _
  var oracleId: String = _
  var location: String = _
  var hireDate: String = _

  def this(un: String) = {
    this()
    this.userName = un
  }

  override def hashCode: Int = {
    var result = 1
    return 31 * result + (if (this.userName == null) 0 else this.userName.hashCode)
  }

  override def equals(obj: Any): Boolean = obj match {
    case person: LDAPPerson => return this.userName.equals(person.userName)
    case _ => false
  }

  override def toString: String = return ToStringBuilder.reflectionToString(this)

}