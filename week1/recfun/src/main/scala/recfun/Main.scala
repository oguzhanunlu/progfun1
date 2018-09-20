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
      if (r == 0 || r == 1) 1
      else if (c == 0 || c == r) 1
      else pascal(c-1, r-1) + pascal(c, r-1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      @scala.annotation.tailrec
      def balanceHelper(lst: List[Char], count: Int): Boolean = {
        if (lst.isEmpty) true
        else if (!lst.contains('(') && !lst.contains(')')) true
        else if (lst.head == '(') balanceHelper(lst.tail, count + 1)
        else if (lst.head == ')')
          if (count > 0) balanceHelper(lst.tail, count -1) else false
        else balanceHelper(lst.tail, count)
      }
      balanceHelper(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (money < 0 || coins.isEmpty) 0
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
