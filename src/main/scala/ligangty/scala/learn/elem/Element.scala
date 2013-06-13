package ligangty.scala.learn.elem

import Element.elem

abstract class Element {
  def contents: Array[String]
  def width: Int = contents(0).length
  def height: Int = contents.length

  def above(that: Element): Element = {
    val this1 = this widen that.width
    val that1 = that widen this.width
    elem(this1.contents ++ that1.contents)
  }

  def beside(that: Element): Element = {
    val this1 = this heighten that.height
    val that1 = that heighten this.height
    elem(for ((line1, line2) <- this1.contents zip that1.contents) yield line1 + line2)
  }

  def widen(w: Int): Element = {
    if (w <= width) this
    else {
      val left = elem(' ', (w - this.width) / 2, this.height)
      val right = elem(' ', w - this.width - left.width, this.height)
      left beside this beside right
    }
  }

  def heighten(h: Int): Element = {
    if (h <= height) this
    else {
      val top = elem(' ', this.width, (h - this.height) / 2)
      val bot = elem(' ', this.width, h - this.height - top.height)
      top above this above bot
    }
  }
  
  override def toString =  this.contents mkString "\n"
}

object Element {
  private class ArrayElement(val contents: Array[String]) extends Element
  private class LineElement(line: String) extends Element {
    val contents = Array(line)
    override def width = contents.length
    override def height = 1
  }
  private class UniformElement(ch: Char, override val width: Int, override val height: Int) extends Element {
    private val line = ch.toString * width
    def contents = Array.make(height, line)
  }

  def elem(contents: Array[String]): Element = new ArrayElement(contents)

  def elem(chr: Char, width: Int, height: Int): Element = new UniformElement(chr, width, height)

  def elem(line: String): Element = new LineElement(line)

}