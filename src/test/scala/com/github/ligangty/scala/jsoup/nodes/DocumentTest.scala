package com.github.ligangty.scala.jsoup.nodes

import java.nio.charset.Charset

import com.github.ligangty.scala.jsoup.nodes.Document.OutputSettings
import com.github.ligangty.scala.jsoup.nodes.Entities.{EXTENDED, BASE}
import org.scalatest.FunSuite

/**
 * test for Document
 */
class DocumentTest extends FunSuite {
  ignore("setTextPreservesDocumentStructure") {
    fail("not implemented!")
  }

  ignore("ignoreTitles") {
    fail("not implemented!")
  }

  ignore("ignoreOutputEncoding") {
    fail("not implemented!")
  }

  ignore("ignoreXhtmlReferences") {
    fail("not implemented!")
  }

  ignore("ignoreNormalisesStructure") {
    fail("not implemented!")
  }

  ignore("ignoreClone") {
    fail("not implemented!")
  }

  ignore("ignoreClonesDeclarations") {
    fail("not implemented!")
  }

  ignore("ignoreLocation() throws IOException") {
    fail("not implemented!")
  }

  ignore("ignoreHtmlAndXmlSyntax") {
    fail("not implemented!")
  }

  ignore("htmlParseDefaultsToHtmlOutputSyntax") {
    fail("not implemented!")
  }

  // Ignored since this ignore can take awhile to run.
  ignore("ignoreOverflowClone") {
    fail("not implemented!")
  }

  test("Document.OutputSettings") {
    val docSetting = new OutputSettings
    assert(Charset.forName("UTF-8") == docSetting.charset)
    assert(BASE == docSetting.escapeMode)
    assert(docSetting.prettyPrint)
    assert(!docSetting.outline)
    assert(docSetting.indentAmount == 1)

    docSetting.charset(Charset.forName("ISO-8859-1")).outline(true).escapeMode(EXTENDED).indentAmount(4).prettyPrint(false)

    assert(Charset.forName("ISO-8859-1") == docSetting.charset)
    assert(EXTENDED == docSetting.escapeMode)
    assert(!docSetting.prettyPrint)
    assert(docSetting.outline)
    assert(docSetting.indentAmount == 4)

  }

}
