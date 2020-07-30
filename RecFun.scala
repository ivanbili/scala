package recfun

import recfun.RecFun.countChange

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
  {
    if (c <= 0 || c >= r)
    {
      if (c == 0 || c == r)
        1
      else
        0
    }
    else
    {
      pascal(c-1, r-1) + pascal(c, r-1)
    }
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balLoop(state: Int, chars: List[Char]): Boolean =
      if (chars.isEmpty) {
        if (state == 0)
          true
        else
          false
      }
      else if (state < 0)
        {
          false
        }
      else {
        if (chars.head == '(')
          balLoop(state + 1, chars.tail)
        else if (chars.head == ')')
          balLoop(state - 1, chars.tail)
        else
          balLoop(state, chars.tail)
      }
    balLoop(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0)
        1
      else if (money < 0 || coins.isEmpty)
        0
      else
        countChange(money - coins.head, coins) + countChange(money, coins.tail)
      }
}
