package com.github.ligangty.scala.jsoup

import com.github.ligangty.scala.jsoup.nodes.Document


class Jsoup {


}

object Jsoup{
  def parse(html:String):Document ={
    //todo not implemented
    null
  }

  /**
   * Creates a new {@link Connection} to a URL. Use to fetch and parse a HTML page.
   * <p>
   * Use examples:
   * <ul>
   * <li><code>Document doc = Jsoup.connect("http://example.com").userAgent("Mozilla").data("name", "jsoup").get();</code></li>
   * <li><code>Document doc = Jsoup.connect("http://example.com").cookie("auth", "token").post();</code></li>
   * </ul>
   * @param url URL to connect to. The protocol must be { @code http} or { @code https}.
   * @return the connection. You can add data, cookies, and headers; set the user-agent, referrer, method; and then execute.
   */
  def connect(url: String): Connection = {
    //todo not implemented
    null
  }
}
