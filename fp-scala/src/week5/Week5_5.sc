package week5

object Week5_5 {
  println("Reduction on Lists.")                  //> Reduction on Lists.
  // Continuation of higher ordered list functions
  //
  // The next class of operations on a list is to combine
  // the elements of a list using a given operator.
  //
  //  So sum(List(x1, ..., xn) = 0 + x1 + ... + xn
  // product(List(x1, ..., xn) = 1 * x1 * ... * xn
  //
  // Sum can be implemented as follows
  def myListSum(xs: List[Int]): Int = xs match {
    case Nil => 0
    case y :: ys => y + myListSum(ys)
  }                                               //> myListSum: (xs: List[Int])Int
  // Testing the above implementation
  myListSum(List(1, 2, 3, 4, 5))                  //> res0: Int = 15

  // We can generalize this pattern using reduceLeft operations
  // List(x1, ... , xn) reduceLeft op = (... (x1 op x2) op ...) op xn

  // Underscore notation
  // Every _ represents a new parameter going from left to right
  // So the anonymous function above can be rewritten as (_ * _)
  //
  // Notice how we can shorten the map() and filter() parameters as well
  // xs map (x => x * 2) becomes xs map (_ * 2)

  // So sum can be generalized as follows
  // def mySumRL(xs: List[Int]): Int = (0 :: xs) reduceLeft((x, y) => x + y)
  def mySumRL(xs: List[Int]): Int = (0 :: xs) reduceLeft(_ + _)
                                                  //> mySumRL: (xs: List[Int])Int
  // def myProdRL(xs: List[Int]): Int = (1 :: xs) reduceLeft((x, y) => x * y)
  def myProdRL(xs: List[Int]): Int = (1 :: xs) reduceLeft(_ * _)
                                                  //> myProdRL: (xs: List[Int])Int
  // Tests
  mySumRL(List(1, 2, 3, 4, 5))                    //> res1: Int = 15
  myProdRL(List(1, 2, 3, 4, 5))                   //> res2: Int = 120

  // There's an even more generalized version of reduceLeft called foldLeft
  // this takes an accumulator (z) followed by an operand later on.
  // And that zero element z is returned if list is empty.
  //
  // (List(x1, ..., xn) foldLeft z)(op) = (... (z op x1) op ...) op xn

  // So now sum and products can be rewritten as
  def mySumFL(xs: List[Int]): Int = (xs foldLeft 0)(_ + _)
                                                  //> mySumFL: (xs: List[Int])Int
  def myProdFL(xs: List[Int]): Int = (xs foldLeft 1)(_ * _)
                                                  //> myProdFL: (xs: List[Int])Int
  mySumFL(List(1, 2, 3, 4, 5))                    //> res3: Int = 15
  myProdFL(List(1, 2, 3, 4, 5))                   //> res4: Int = 120

  // Here's a possible implementation of foldLeft and reduceLeft inside List
  // abstract class List[T] {
  //   ...
  //   def foldLeft[U](z: U)(op: (U, T) => U): U = this match {
  //     case Nil => z
  //     case x :: xs => (xs foldLeft op(z, x))(op)
  //   }
  //   ...
  //   def reduceLeft[T](op: (T, T) => T): T = this match {
  //     case Nil => throw new Error("Nil.reduceLeft")
  //     case x :: xs => (xs foldLeft x)(op)
  //   }
  //   ...
  // }

  // The above two were operations that lean to the left and expressions are
  // evaluated from left bottom. We have a similar set of operations where the
  // trees lean right and evaluation starts from right bottom.

  // List(x1, .., x{n-1}, xn) reduceRight op = x1 op (... (x{n-1} op xn) ...)
  // (List(x1, .., xn) foldRight z)(op)      = x1 op (... (xn op z) ...)

  // Their implementation would look like the follows
  //
  // abstract class List[T] {
  //   ...
  //   def foldRight[U](z: U)(op: (T, U) => U): U = this match {
  //     case Nil => z
  //     case x :: xs => op(x, (xs foldRight z)(op))
  //   }
  //   ...
  //   def reduceRight[T](op: (T, T) => T): T = this match {
  //     case Nil => throw new Error("Nil.reduceLeft")
  //     case x :: xs => op(x, xs.reduceRight(op))
  //   }
  //   ...
  // }

  // For operators which are associative and commutative
  // foldRight and foldLeft are equivalent.
  // Though, there might be a difference in efficiency.
  // But sometimes, only one of the two operators is appropriate.

  // Exercise
  // Concatenation function that joins two lists
  def concat[T](xs: List[T], ys: List[T]): List[T] = (xs foldRight ys)(_ :: _)
  concat(List(1, 2, 3), List(4, 5, 6))
  // You cannot replace foldLeft with foldRight above.
  // Because :: cannot be applied on (List[T],T), it can only be applied on
  // (T, List[T])
}