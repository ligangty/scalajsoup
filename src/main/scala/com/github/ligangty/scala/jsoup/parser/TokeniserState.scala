package com.github.ligangty.scala.jsoup.parser


sealed trait TokeniserState {
  def read(t: Tokeniser, r: CharacterReader): Unit
}

case object Data extends TokeniserState {
  def read(t: Tokeniser, r: CharacterReader): Unit = {
    //    r.current match {
    //      case '&' =>
    //        t.advanceTransition(CharacterReferenceInData)
    //      case '<' =>
    //        t.advanceTransition(TagOpen)
    //      case nullChar =>
    //        t.error(this)
    //        t.emit(r.consume)
    //      case eof =>
    //        t.emit(new Token.EOF)
    //        break //todo: break is not supported
    //      case _ =>
    //        val data: String = r.consumeData
    //        t.emit(data)
    //    }
  }
}
