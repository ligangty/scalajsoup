package com.github.ligangty.scala.jsoup.integration

import java.io.{UnsupportedEncodingException, ByteArrayInputStream, InputStream, File}
import java.net.URISyntaxException

import com.github.ligangty.scala.jsoup.Jsoup
import com.github.ligangty.scala.jsoup.nodes.Document
import com.github.ligangty.scala.jsoup.select.Elements
import org.scalatest.FunSuite
import ParseTest._

/**
 * Integration test: parses from real-world example HTML.
 *
 */
class ParseTest extends FunSuite {

  test("testSmhBizArticle") {
    val in: File = getFile("/htmltests/smh-biz-article-1.html")
    val doc: Document = Jsoup.parse(in, "UTF-8", "http://www.smh.com.au/business/the-boards-next-fear-the-female-quota-20100106-lteq.html")
    assert("The board’s next fear: the female quota" == doc.title)
    //    assert("en"==doc.select("html").attr("xml:lang"))
    //    val articleBody: Elements = doc.select(".articleBody > *")
    //    assert(17==articleBody.size)
  }
}

object ParseTest {

  def getFile(resourceName: String): File = {
    try {
      val file: File = new File(classOf[ParseTest].getResource(resourceName).toURI)
      file
    } catch {
      case e: URISyntaxException =>
        throw new IllegalStateException(e)
    }
  }

  def inputStreamFrom(s: String): InputStream = {
    try {
      new ByteArrayInputStream(s.getBytes("UTF-8"))
    }
    catch {
      case e: UnsupportedEncodingException =>
        throw new RuntimeException(e)
    }
  }
}
