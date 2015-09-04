package week6

object Week6_3 {
  println("Sets and Advanced For-Expressions")    //> Sets and Advanced For-Expressions
  // Sets
  // Sets are very close to sequences
  // syntax as follows
  val fruits = Set("apple", "banana", "pear")     //> fruits  : scala.collection.immutable.Set[String] = Set(apple, banana, pear)
  val s = (1 to 6).toSet                          //> s  : scala.collection.immutable.Set[Int] = Set(5, 1, 6, 2, 3, 4)
  // Most operations present on sequences are available for Sets as well...
  s map (_ + 2)                                   //> res0: scala.collection.immutable.Set[Int] = Set(5, 6, 7, 3, 8, 4)
  fruits filter (_.startsWith("app"))             //> res1: scala.collection.immutable.Set[String] = Set(apple)
  s.nonEmpty                                      //> res2: Boolean = true
  // Set is a subclass 'Iterables'.

  // Important Properties of Sets:
  // 1) Sets are unordered (notice the ordering of elements)
  s                                               //> res3: scala.collection.immutable.Set[Int] = Set(5, 1, 6, 2, 3, 4)
  // 2) Sets don't have duplicate elements
  s map (_ / 2)                                   //> res4: scala.collection.immutable.Set[Int] = Set(2, 0, 3, 1)
  // 3) Fundamental operation on sets is 'contains'
  s contains 5                                    //> res5: Boolean = true
  s contains 20                                   //> res6: Boolean = false

  // Combinatorial Search problem: N-Queens
  // https://developers.google.com/optimization/puzzles/queens
  //
  // Algorithm to solve this:
  // 1) Suppose we've already generated all the solutions consisting of placing
  //    k - 1 queens on a board size of n
  // 2) Each solution is represented by a list (of length k - 1) containing the
  //    number of columns (between 0 and n - 1)
  // 3) The column number of the queen in the (k-1)th row comes first in the list,
  //    followed by the column number of the queen in row k - 2, etc.
  // 4) The solution set is thus represented as a set of lists with one element
  //    for each solution.
  // 5) Now, to place the kth queen, we generate all possible extensions of each
  //    solution preceded by a new queen.
  // The solution is as follows
  def nqueens(n: Int): Set[List[Int]] = {
    def isSafe(col: Int, queens: List[Int]): Boolean = {
      val row = queens.length
      val queensWithRow = (row - 1 to 0 by -1).zip(queens)
      queensWithRow forall {
        case (r, c) => (col != c) && (math.abs(col - c) != (row - r))
      }
    }
    def placeQueens(k: Int): Set[List[Int]] = {
      if (k == 0) {
         Set(List())
      } else {
        for {
          queens <- placeQueens(k - 1)
          col <- 0 until n
          if (isSafe(col, queens))
        } yield col :: queens
      }
    }
    placeQueens(n)
  }                                               //> nqueens: (n: Int)Set[List[Int]]
  // Test for N-Queens
  nqueens(1)                                      //> res7: Set[List[Int]] = Set(List(0))
  nqueens(2)                                      //> res8: Set[List[Int]] = Set()
  nqueens(3)                                      //> res9: Set[List[Int]] = Set()
  nqueens(4)                                      //> res10: Set[List[Int]] = Set(List(1, 3, 0, 2), List(2, 0, 3, 1))
}