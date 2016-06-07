package recfun

import scala.util.Try

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def balance(chars: List[Char], stack: List[Boolean]): Boolean = {
      if (chars.isEmpty) {
        stack.isEmpty
      } else if (chars.head == '(') {
        balance(chars.tail, true :: stack)
      } else if (chars.head == ')') {
        balance(chars.tail, stack.tail)
      } else {
        balance(chars.tail, stack)
      }
    }
    Try(balance(chars, List.empty)).getOrElse(false)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    val sorted = coins.sortWith((x, y) => x > y)

    def count(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (coins.isEmpty) 0
      else {
        if (coins.head > money) count(money, coins.tail)
        else count(money - coins.head, coins) + count(money, coins.tail)
      }
    }

    count(money, sorted)
  }
}
