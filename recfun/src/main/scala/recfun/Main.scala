package recfun

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
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || r <= 1 || c == r)
      1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def loop(chars: List[Char], open: Int): Boolean =
      if (chars.isEmpty)
        open == 0
      else if (chars.head == '(')
        loop(chars.tail, open + 1)
      else if (chars.head == ')' && open > 0)
        loop(chars.tail, open - 1)
      else if (chars.head == ')' && open <= 0)
        false
      else
        loop(chars.tail, open)
    loop(chars, 0)

  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def loop(coins: List[Int], accumulator: Int): Int =
      if (coins.isEmpty)
        0
      else if (accumulator > money)
        0
      else if (accumulator == money)
        1
      else
        loop(coins.tail, accumulator) +
          loop(coins, accumulator + coins.head)
    if (money <= 0) 0 else loop(coins, 0)
  }
}
