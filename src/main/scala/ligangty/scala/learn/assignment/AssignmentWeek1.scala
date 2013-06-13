package ligangty.scala.learn

import scala.collection.mutable.Stack

object AssignmentWeek1 {
  // Assignment 1 Testing
  //  printPascal(9)

  // Assignment 2 Testing
  val testString = "kljfsf()ksjf(())skldfj(fskjdf(slkdfjlsdf)(jskdfj(skdf)))__()"
  println(balance(testString.toList))

  // Assignment 3 Testing
  //  val money = 10
  //  val coins = List(2,2,1,5)

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
    def balanceInner(flag: Int, chars: List[Char]): Boolean = {
      if ((flag < 0) || (chars.isEmpty && flag > 0))
        false
      else if (chars.isEmpty && flag == 0)
        true
      else {
        if (chars.head == '(')
          balanceInner(flag + 1, chars.tail)
        else if (chars.head == ')')
          balanceInner(flag - 1, chars.tail)
        else
          balanceInner(flag, chars.tail)
      }
    }

    balanceInner(0, chars)
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

  //Assignment 3
  def countChange(money: Int, coins: List[Int]): Int = {
    coins.sorted
    require(money > 0)
    1
  }
  
}