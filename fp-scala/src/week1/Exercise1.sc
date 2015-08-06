package week1

object Exercise1 {
  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c, r - 1) + pascal(c - 1, r - 1)
  }                                               //> pascal: (c: Int, r: Int)Int
  // test
  def printPascal() = {
    println("Pascal's Triangle")
    for (row <- 0 to 5) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }                                               //> printPascal: ()Unit
  printPascal()                                   //> Pascal's Triangle
                                                  //| 1 
                                                  //| 1 1 
                                                  //| 1 2 1 
                                                  //| 1 3 3 1 
                                                  //| 1 4 6 4 1 
                                                  //| 1 5 10 10 5 1 

  /**
   * Exercise 2
   */
  def balance(ch: List[Char]): Boolean = {
    def iter(ch: List[Char], count: Int): Boolean = {
      if (ch.isEmpty)
        count == 0
      else if (ch.head == '(')
        iter(ch.tail, count + 1)
      else if (ch.head == ')')
        if (count > 0) iter(ch.tail, count - 1)
        else false
      else
        iter(ch.tail, count)
    }
    iter(ch, 0)
  }                                               //> balance: (ch: List[Char])Boolean

  // tests
  balance("(if (zero? x) max (/ 1 x))".toList) == true
                                                  //> res0: Boolean = true
  balance("I told him (that it's not (yet) done).\n(But he wasn't listening)".toList) == true
                                                  //> res1: Boolean = true
  balance(":-)".toList) == false                  //> res2: Boolean = true
  balance("())(".toList) == false                 //> res3: Boolean = true
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money < 0) { // money exhausted and negative?
      0
    } else if (coins.isEmpty) { // you have coins or not?
      if (money == 0) 1 else 0
    } else {
      countChange(money, coins.tail) +
      countChange(money - coins.head, coins)
    }
  }                                               //> countChange: (money: Int, coins: List[Int])Int
  
  //tests
  countChange(4,List(1,2)) == 3                   //> res4: Boolean = true
  countChange(300,List(5,10,20,50,100,200,500)) == 1022
                                                  //> res5: Boolean = true
  countChange(301,List(5,10,20,50,100,200,500)) == 0
                                                  //> res6: Boolean = true
  countChange(300,List(500,5,50,100,20,200,10)) == 1022
                                                  //> res7: Boolean = true
}
