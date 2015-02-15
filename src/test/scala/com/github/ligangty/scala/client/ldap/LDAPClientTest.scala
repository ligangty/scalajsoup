package com.github.ligangty.scala.client.ldap

import org.scalatest.FunSuite

class LDAPClientTest extends FunSuite{
  test("simple test"){
    val client:LDAPClient = new LDAPClient("ldap://ldap.yourdomain.com")
    val person:LDAPPerson = client.findPersonByName("username")
    assert(person!=null)
    assert(person.userName=="username")
    assert(person.realName=="realname")
    assert(person.email=="username@yourdomain.com")
  }
}
