package week6

object Week6_1 {
  println("Other collections")                    //> Other collections
  // Till now we've seen lists (immutable)

  // Lists are linear. Access to first element is much
  // faster than access to the middle or the end of the list

  // Scala library also defines an alternative sequence implementation
  // called "Vector". This one has more evenly balanced access
  // pattern than List.

  // A vector is implemented as a very shallow tree. Whose tree node have 32
  // pointers, tail nodes have 32 elements. So we'd have a 32-ary tree.
  // With every level of indirection increase we multiply the number of nodes
  // by 2^5 or 32. Time taken to access an element is dependant on log_32_N.

  val nums = Vector(1, 2, 3, 4, 5)                //> nums  : scala.collection.immutable.Vector[Int] = Vector(1, 2, 3, 4, 5)
  // You can apply almost all list operations on Vectors as well. Except cons
  // :: is the building block of List and is not related to Vectors. Instead
  // you could do +: that adds a new element to the start of the vector and :+
  // that adds an element to the end of the vector. Rule to remember, you
  // have : always facing the sequence.

  // Vectors are IMMUTABLE.

  // Vectors and Lists are an implementation of sequences (Seq). Seq itself is
  // a subclass of Iterable. Other subclasses of Iterable are Set and Map.

  // Arrays and strings are mutable. They can be implicitly converted to sequences
  // when needed. These cannot be subclasses of Seq as they come from Java.

  // Another sequence like structres are Range. They are evenly spaced integers.
  // There are three operators associated with a range.
  // 1) to (includsive)
  // 2) until (exclusive)
  // 3) by (determine the step value).
  // Some examples:
  1 to 10 by 3                                    //> res0: scala.collection.immutable.Range = Range(1, 4, 7, 10)
  6 until 1 by -2                                 //> res1: scala.collection.immutable.Range = Range(6, 4, 2)

  // We just need to store lower bound, upper bound and the step value.

  // So now that we know about sequences let's look at more operations on
  // them.

  val xs = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)    //> xs  : List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  val ys = List(-1, -2, -3, -4, -5, -6, -7, -8, -9, -10)
                                                  //> ys  : List[Int] = List(-1, -2, -3, -4, -5, -6, -7, -8, -9, -10)

  // Says whether there's an element in xs which satisfies p(x) == true
  xs exists (_ % 2 == 0)                          //> res2: Boolean = true

  // Says whether p(x) == true for all x belongs to xs.
  xs forall (_ > 0)                               //> res3: Boolean = true

  // Returns a sequence of corresponding (x, y) for every x, y belonging to xs, ys
  val myZip = xs zip ys                           //> myZip  : List[(Int, Int)] = List((1,-1), (2,-2), (3,-3), (4,-4), (5,-5), (6
                                                  //| ,-6), (7,-7), (8,-8), (9,-9), (10,-10))

  // The dual of ZIP is unzip
  myZip.unzip                                     //> res4: (List[Int], List[Int]) = (List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),List(-1
                                                  //| , -2, -3, -4, -5, -6, -7, -8, -9, -10))

  // Sum of all elements of the sequence.
  xs.sum                                          //> res5: Int = 55

  // The product of all elements of the sequence
  xs.product                                      //> res6: Int = 3628800

  // Maximum element of the sequence
  xs.max                                          //> res7: Int = 10

  // Minimum element of the sequence
  xs.min                                          //> res8: Int = 1

  // flatMap: Applies collection-valued function f to all elements of xs
  // and then concatenates the results. This is particularly useful when
  // every x in xs returns a list ys. And these need to be concatenated
  // into a single list.
  xs flatMap (x => 1 to x)                        //> res9: List[Int] = List(1, 1, 2, 1, 2, 3, 1, 2, 3, 4, 1, 2, 3, 4, 5, 1, 2, 3
                                                  //| , 4, 5, 6, 1, 2, 3, 4, 5, 6, 7, 1, 2, 3, 4, 5, 6, 7, 8, 1, 2, 3, 4, 5, 6, 7
                                                  //| , 8, 9, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

  // Generate all combinations of numbers x and y where X is drawn from 1..M
  // and y is drawn from 1..N
  (1 to 3) flatMap(x => (1 to 2) map (y => (x, y)))
                                                  //> res10: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((1,1), (1
                                                  //| ,2), (2,1), (2,2), (3,1), (3,2))

  // Scalar product of two vectors
  def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double = {
    // (xs zip ys).map(xy => xy._1 * xy._2).sum
    // Another trick:
    // Use pattern matching on the function value
    (xs zip ys).map{ case (x, y) => x * y }.sum
  }                                               //> scalarProduct: (xs: Vector[Double], ys: Vector[Double])Double

  // Generally, the function value
  // { case p1 => e1 ... case pn => en }
  // is equivalent to
  // x => x match { case p1 => e1 ... case pn => en }

  // Exercise:
  // What is a good high level test to write whether a number
  // is a prime number or not. A number is a prime number the only
  // divisors are 1 and n.
  // Value conciseness rather than efficiency for the below.
  def isPrime(n: Int): Boolean = (2 until n) forall (n % _ != 0)
                                                  //> isPrime: (n: Int)Boolean
  isPrime(3)                                      //> res11: Boolean = true
  isPrime(4)                                      //> res12: Boolean = false
  isPrime(9)                                      //> res13: Boolean = false
  isPrime(11)                                     //> res14: Boolean = true
}