package com.github.ligangty.scala.client.ldap

import org.scalatest.FunSuite

class LDAPPersonTest extends FunSuite {

  test("Simple Test") {
    val person1: LDAPPerson = LDAPPerson("username")
    person1.realName = "User Name"
    assert(null != person1)
    assert(person1.equals(person1))
    assert(!person1.equals(null))
    val person2: LDAPPerson = LDAPPerson("username")
    assert(person1.equals(person2))
    val person3: LDAPPerson = LDAPPerson("username1")
    assert(!person1.equals(person3))

    println(person1.toString)
  }
}