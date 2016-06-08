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
    def balance(chars: List[Char], counter: Int): Boolean = {
      if (chars.isEmpty) {
        counter == 0
      } else if (chars.head == '(') {
        balance(chars.tail, counter + 1)
      } else if (chars.head == ')') {
        if (counter == 0) throw new RuntimeException("unbalanced")
        else balance(chars.tail, counter - 1)
      } else {
        balance(chars.tail, counter)
      }
    }
    Try(balance(chars, 0)).getOrElse(false)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (coins.isEmpty) 0
    else {
      if (coins.head > money) countChange(money, coins.tail)
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
}
