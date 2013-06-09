package ligangty.scala.learn

import scala.collection.mutable.Stack

object Learn extends App {
  // Assignment 1 Testing
  //  printPascal(9)

  // Asignment 2 Testing
  val testString = "(((())))example"
  println(balanceStacking(testString.toList))

  // Assignment 1:
  def pascal(c: Int, r: Int): Int = {
    require(c <= r)
    if (c == 0) 1 else if (c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  def printPascal(rows: Int) {
    for (
      i <- 0 to rows;
      j <- 0 to i
    ) if ((j != 0 && j == i) || (j == 0 && i == 0))
      println(pascal(j, i))
    else
      print(pascal(j, i) + " ")
  }

  // Assignment 2: recursive way
  def balance(chars: List[Char]): Boolean = {
    false
  }

  // Assignment 2: stack way
  def balanceStacking(chars: List[Char]): Boolean = {
    val stack = new Stack[Char]()
    for (char <- chars) {
      if (char == '(')
        stack.push(char)
      else if (char == ')' && !stack.isEmpty)
        stack.pop
      else if (char == ')' && stack.isEmpty)
        return false
    }
    stack.isEmpty
  }

}