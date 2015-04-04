package com.github.ligangty.scala.jsoup.nodes

import org.scalatest.FunSuite

/**
 * Tests for the DocumentType node
 */
class DocumentTypeTest extends FunSuite {

  test("constructorValidationOkWithBlankName") {
    val fail: DocumentType = new DocumentType("", "", "", "")
  }

  test("constructorValidationThrowsExceptionOnNulls") {
    intercept[IllegalArgumentException] {
      val fail: DocumentType = new DocumentType("html", null, null, "")
    }
  }

  test("constructorValidationOkWithBlankPublicAndSystemIds") {
    val fail: DocumentType = new DocumentType("html", "", "", "")
  }

  test("outerHtmlGeneration") {
    val html5: DocumentType = new DocumentType("html", "", "", "")
    assert("<!doctype html>" == html5.outerHtml)
    val publicDocType: DocumentType = new DocumentType("html", "-//IETF//DTD HTML//", "", "")
    assert("<!DOCTYPE html PUBLIC \"-//IETF//DTD HTML//\">" == publicDocType.outerHtml)
    val systemDocType: DocumentType = new DocumentType("html", "", "http://www.ibm.com/data/dtd/v11/ibmxhtml1-transitional.dtd", "")
    assert("<!DOCTYPE html \"http://www.ibm.com/data/dtd/v11/ibmxhtml1-transitional.dtd\">" == systemDocType.outerHtml)
    val combo: DocumentType = new DocumentType("notHtml", "--public", "--system", "")
    assert("<!DOCTYPE notHtml PUBLIC \"--public\" \"--system\">" == combo.outerHtml)
  }
}
