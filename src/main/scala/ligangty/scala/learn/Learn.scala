package ligangty.scala.learn
import ligangty.scala.learn.elem.Spiral.spiral
object Learn extends App {
  def testArrayMethods = {
    val a1: Array[String] = Array("1", "2", "3")
    val a2: Array[String] = Array("4", "5", "6")
    (a1 ++ a2).foreach(println _)
    (a1 zip a2).foreach(println _)
    (for ((l1, l2) <- a1 zip a2) yield l1 + l2).foreach(println _)
    Array.make(6, ("a" * 5)).foreach(println _)
  }
  
  def testSpiral = {
    val nSides = 20
    println(spiral(nSides,0))
  }

  testSpiral
}