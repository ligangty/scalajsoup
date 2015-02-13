package com.github.ligangty.scala.client.ldap

import org.scalatest.FunSuite

class LDAPPersonTest extends FunSuite {

  test("Simple Test") {
    val person1: LDAPPerson = new LDAPPerson("gli")
    person1.realName = "Gang Li"
    assert(null != person1)
    assert(person1.equals(person1))
    assert(!person1.equals(null))
    val person2: LDAPPerson = new LDAPPerson("gli")
    assert(person1.equals(person2))
    val person3: LDAPPerson = new LDAPPerson("xuliu")
    assert(!person1.equals(person3))
    
    println(person1.toString)
  }
}