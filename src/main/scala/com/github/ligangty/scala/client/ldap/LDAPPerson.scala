package com.github.ligangty.scala.client.ldap

import org.apache.commons.lang3.builder.ToStringBuilder

case class LDAPPerson(userName:String) {

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
    val prime = 31;
    var result = 1;
    result = prime * result + this.userName.hashCode;
    return result;
  }

  override def equals(obj: Any): Boolean = obj match {
    case person: LDAPPerson => return this.userName.equals(person.userName)
    case _ => false
  }

  override def toString: String = return ToStringBuilder.reflectionToString(this);

}