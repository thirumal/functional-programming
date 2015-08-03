object Exercise1 {
  /** Private method to print the pascal's triangle */
  private def printPascal() = {
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
    if (r == 1 || r == c) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }
  // test
  printPascal()

  /**
   * Exercise 2
   */
  def balance(ch: List[Char]): Boolean = {
    def iter(ch: List[Char], count: Int): Boolean = {
      if (ch.isEmpty)
        true
      else if (ch.head == '(')
        iter(ch.tail, count + 1)
      else if (ch.head == ')')
        if (count > 0) iter(ch.tail, count - 1)
        else false
      else
        iter(ch.tail, count)
    }
    iter(ch, 0)
  }

  // tests
  balance("(if (zero? x) max (/ 1 x))".toList) == true
  balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList) == true
  balance(":-)".toList) == false
  balance("())(".toList) == false
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = 0
  
  //tests
  // countChange(4,List(1,2)) == 3
  // countChange(300,List(5,10,20,50,100,200,500)) == 1022
  // countChange(301,List(5,10,20,50,100,200,500)) == 0
  // countChange(300,List(500,5,50,100,20,200,10)) == 1022
}