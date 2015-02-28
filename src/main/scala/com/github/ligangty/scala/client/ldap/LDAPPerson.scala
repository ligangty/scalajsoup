package com.github.ligangty.scala.client.ldap

import org.apache.commons.lang3.builder.ToStringBuilder

class LDAPPerson(val userName:String) {

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

  override def hashCode: Int = {
    var result = 1
    31 * result + (if (this.userName == null) 0 else this.userName.hashCode)
  }

  override def equals(obj: Any): Boolean = obj match {
    case person: LDAPPerson => this.userName.equals(person.userName)
    case _ => false
  }

  override def toString: String = ToStringBuilder.reflectionToString(this)

}