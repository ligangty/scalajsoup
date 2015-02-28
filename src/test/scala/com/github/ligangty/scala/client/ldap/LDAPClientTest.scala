package com.github.ligangty.scala.client.ldap

import org.scalatest.FunSuite

class LDAPClientTest extends FunSuite{
  test("simple test"){
    val client:LDAPClient = new LDAPClient("ldap://ldap.corp.yourdomain.com")
    val person:LDAPPerson = client.findPersonByName("gli")
    assert(person!=null)
    assert(person.userName=="username")
    assert(person.realName=="real name")
    assert(person.email=="username@yourdomain.com")
  }
}
