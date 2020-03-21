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
  def pascal(c: Int, r: Int): Int = {
    if (c == 0)
      1
    else if (r == 0 || r < c)
      0
    else
      pascal(c,r-1)+pascal(c-1,r-1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def auxBalance(chars: List[Char], openingParan: Int, closingParan: Int): Boolean = {
      if (chars.isEmpty)
        (openingParan == closingParan)
      else {
        if (closingParan > openingParan)
          false
        else
        {
          val c: Char = chars.head
          c match {
            case '(' => auxBalance(chars.tail, openingParan + 1, closingParan)
            case ')' => auxBalance(chars.tail, openingParan, closingParan + 1)
            case _ => auxBalance(chars.tail, openingParan, closingParan)
          }
        }
      }
    }
    auxBalance(chars, 0, 0)
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty)
      0
    else {
      val h: Int = coins.head
      if (money - h == 0)
        1 + countChange(money, coins.tail)
      else if (money - h < 0)
        countChange(money, coins.tail)
      else
        countChange(money-h, coins) +
          countChange(money, coins.tail)
    }
  }
}
