package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }

    println(balance("(if (zero? x) max (/ 1 x))".toList))
    println(balance("I told him (that it’s not (yet) done). (But he wasn’t listening)".toList))
    println(balance(":-)".toList))
    println(balance("())(".toList))

    println(countChange(4, List(4,4,4,4)))
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    if (r <= 1 || c == r || c == 0) 1 else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def balanceIter(bal: Int, chars: List[Char]): Int = {
      if (chars.isEmpty) bal else {
        if (chars.head == ')' && bal <= 0)
        // if we ever encounter a ) without any preceeding ( it is already unbalanced, skip the rest
          balanceIter(-1, List[Char]())
        else if (chars.head == '(')
          balanceIter(bal + 1, chars.tail)
        else if (chars.head == ')')
          balanceIter(bal - 1, chars.tail)
        else
          balanceIter(bal, chars.tail)
      }
    }

    balanceIter(0, chars) == 0
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChangeIter(counter: Int, money: Int, coins: List[Int]): Int = {

      if (coins.isEmpty || money == 0) counter
      else if (money==coins.head)
        countChangeIter(counter+1, money, coins.tail)
      else if (money>coins.head)
        countChangeIter(counter, money - coins.head, coins) + countChangeIter(counter, money, coins.tail)
      else
        countChangeIter(counter, money, coins.tail)
    }
    countChangeIter(0, money, coins.distinct.sorted)
  }
}
