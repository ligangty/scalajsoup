package com.github.ligangty.scala.jsoup.integration

import java.io.{UnsupportedEncodingException, ByteArrayInputStream, InputStream, File}
import java.net.URISyntaxException

import com.github.ligangty.scala.jsoup.Jsoup
import com.github.ligangty.scala.jsoup.nodes.{Element, Document}
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
    assert("en" == doc.select("html").attr("xml:lang"))
    val articleBody: Elements = doc.select(".articleBody > *")
    assert(17 == articleBody.size)
  }

  test("testNewsHomepage") {
    val in: File = getFile("/htmltests/news-com-au-home.html")
    val doc: Document = Jsoup.parse(in, "UTF-8", "http://www.news.com.au/")
    assert("News.com.au | News from Australia and around the world online | NewsComAu" == doc.title)
    assert("Brace yourself for Metro meltdown" == doc.select(".id1225817868581 h4").text.trim)
    val a: Element = doc.select("a[href=/entertainment/horoscopes]").first()
    assert("/entertainment/horoscopes" == a.attr("href"))
    assert("http://www.news.com.au/entertainment/horoscopes" == a.attr("abs:href"))
    val hs: Element = doc.select("a[href*=naughty-corners-are-a-bad-idea]").first()
    assert("http://www.heraldsun.com.au/news/naughty-corners-are-a-bad-idea-for-kids/story-e6frf7jo-1225817899003" == hs.attr("href"))
    assert(hs.attr("href") == hs.attr("abs:href"))
  }

  test("testGoogleSearchIpod") {
    val in: File = getFile("/htmltests/google-ipod.html")
    val doc: Document = Jsoup.parse(in, "UTF-8", "http://www.google.com/search?hl=en&q=ipod&aq=f&oq=&aqi=g10")
    assert("ipod - Google Search" == doc.title)
    val results: Elements = doc.select("h3.r > a")
    assert(12 == results.size)
    assert("http://news.google.com/news?hl=en&q=ipod&um=1&ie=UTF-8&ei=uYlKS4SbBoGg6gPf-5XXCw&sa=X&oi=news_group&ct=title&resnum=1&ved=0CCIQsQQwAA" == results.get(0).attr("href"))
    assert("http://www.apple.com/itunes/" == results.get(1).attr("href"))
  }

  test("testBinary") {
    val in: File = getFile("/htmltests/thumb.jpg")
    val doc: Document = Jsoup.parse(in, "UTF-8")
    assert(doc.text().contains("gd-jpeg"))
  }

  test("testYahooJp") {
    val in: File = getFile("/htmltests/yahoo-jp.html")
    val doc: Document = Jsoup.parse(in, "UTF-8", "http://www.yahoo.co.jp/index.html")
    assert("Yahoo! JAPAN" == doc.title)
    val a: Element = doc.select("a[href=t/2322m2]").first()
    assert("http://www.yahoo.co.jp/_ylh=X3oDMTB0NWxnaGxsBF9TAzIwNzcyOTYyNjUEdGlkAzEyBHRtcGwDZ2Ex/t/2322m2" == a.attr("abs:href"))
    assert("全国、人気の駅ランキング" == a.text)
  }

  test("testBaidu") {
    val in: File = getFile("/htmltests/baidu-cn-home.html")
    val doc: Document = Jsoup.parse(in, null, "http://www.baidu.com/")
    var submit: Element = doc.select("#su").first()
    assert("百度一下" == submit.attr("value"))
    submit = doc.select("input[value=百度一下]").first()
    assert("su" == submit.id)
    val newsLink: Element = doc.select("a:contains(新)").first()
    assert("http://news.baidu.com" == newsLink.absUrl("href"))
    assert("GB2312" == doc.outputSettings.charset.displayName)
    assert("<title>百度一下，你就知道      </title>" == doc.select("title").outerHtml)
    doc.outputSettings.charset("ascii")
    assert("<title>&#x767e;&#x5ea6;&#x4e00;&#x4e0b;&#xff0c;&#x4f60;&#x5c31;&#x77e5;&#x9053;      </title>" == doc.select("title").outerHtml)
  }

  test("testBaiduVariant") {
    val in: File = getFile("/htmltests/baidu-variant.html")
    val doc: Document = Jsoup.parse(in, null, "http://www.baidu.com/")
    assert("GB2312" == doc.outputSettings.charset.displayName)
    assert("<title>百度一下，你就知道</title>" == doc.select("title").outerHtml)
  }

  test("testHtml5Charset") {
    var in: File = getFile("/htmltests/meta-charset-1.html")
    var doc: Document = Jsoup.parse(in, null, "http://example.com/")
    assert("新" == doc.text())
    assert("GB2312" == doc.outputSettings.charset.displayName)
    in = getFile("/htmltests/meta-charset-2.html")
    doc = Jsoup.parse(in, null, "http://example.com")
    assert("UTF-8" == doc.outputSettings.charset.displayName)
    assert("新" != doc.text())
    in = getFile("/htmltests/meta-charset-3.html")
    doc = Jsoup.parse(in, null, "http://example.com/")
    assert("UTF-8" == doc.outputSettings.charset.displayName)
    assert("新" == doc.text())
  }

  test("testBrokenHtml5CharsetWithASingleDoubleQuote") {
    val in: InputStream = inputStreamFrom("<html>\n" + "<head><meta charset=UTF-8\"></head>\n" + "<body></body>\n" + "</html>")
    val doc: Document = Jsoup.parse(in, null, "http://example.com/")
    assert("UTF-8" == doc.outputSettings.charset.displayName)
  }

  test("testNytArticle") {
    val in: File = getFile("/htmltests/nyt-article-1.html")
    val doc: Document = Jsoup.parse(in, null, "http://www.nytimes.com/2010/07/26/business/global/26bp.html?hp")
    val headline: Element = doc.select("nyt_headline[version=1.0]").first()
    assert("As BP Lays Out Future, It Will Not Include Hayward" == headline.text())
  }

  test("testYahooArticle") {
    val in: File = getFile("/htmltests/yahoo-article-1.html")
    val doc: Document = Jsoup.parse(in, "UTF-8", "http://news.yahoo.com/s/nm/20100831/bs_nm/us_gm_china")
    val p: Element = doc.select("p:contains(Volt will be sold in the United States").first()
    assert("In July, GM said its electric Chevrolet Volt will be sold in the United States at $41,000 -- $8,000 more than its nearest competitor, the Nissan Leaf." == p.text())
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
    } catch {
      case e: UnsupportedEncodingException =>
        throw new RuntimeException(e)
    }
  }
}
