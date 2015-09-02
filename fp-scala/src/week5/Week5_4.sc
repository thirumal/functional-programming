package week5

object Week5_4 {
  println("Higher order operations on list")      //> Higher order operations on list
  // Common operations on fall into these categories
  //
  // 1) Transforming each element in a list in a certain way
  // 2) Retrieving a list of all elements satisfying a certain condition
  // 3) Combining the elements of a list using an operator

  // An example list to be used
  val myList = List(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5)
                                                  //> myList  : List[Int] = List(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5)

  // First, transform every element of a list
  // An example would be to scale all list's elements by some factor
  def scaleList1(xs: List[Double], factor: Double): List[Double] = xs match {
    case Nil => Nil
    case y :: ys => (y * factor) :: scaleList1(ys, factor)
  }                                               //> scaleList1: (xs: List[Double], factor: Double)List[Double]
  scaleList1(List(1.0, 2.0, 3.0), 5.0)            //> res0: List[Double] = List(5.0, 10.0, 15.0)

  // This scheme can be generalized using the 'map' method on List
  //
  // abstract class List[T] { ...
  //   def map[U](f: T => U): List[U] = this match {
  //     case Nil => this
  //     case x :: xs => f(x) :: xs.map(f)
  //   }
  //   ...
  // }

  // So scaleList can actually be defined as:
  def scaleList(xs: List[Double], factor: Double): List[Double] = {
    xs.map(x => x * factor)
  }                                               //> scaleList: (xs: List[Double], factor: Double)List[Double]
  scaleList(List(1.0, 2.0, 3.0), 0.5)             //> res1: List[Double] = List(0.5, 1.0, 1.5)

  // Exercise
  // squareList Implemenation
  def sqList(xs: List[Int]): List[Int] = xs match {
    case Nil => xs
    case y :: ys => (y * y) :: sqList(ys)
  }                                               //> sqList: (xs: List[Int])List[Int]
  sqList(myList)                                  //> res2: List[Int] = List(25, 16, 9, 4, 1, 0, 1, 4, 9, 16, 25)
  // Using map, the same function
  def squareList(xs: List[Int]) = xs map (x => x * x)
                                                  //> squareList: (xs: List[Int])List[Int]
  squareList(myList)                              //> res3: List[Int] = List(25, 16, 9, 4, 1, 0, 1, 4, 9, 16, 25)

  // Filtering..
  // Selecting all elements which match a certain condition
  // returnonly the positive elements of a list
  def posElems1(xs: List[Int]): List[Int] = xs match {
    case Nil => Nil
    case y :: ys => if(y > 0) y :: posElems1(ys) else posElems1(ys)
  }                                               //> posElems1: (xs: List[Int])List[Int]
  posElems1(myList)                               //> res4: List[Int] = List(1, 2, 3, 4, 5)

  // Again this method can be generalized to a method filter() in the List class
  //
  // This scheme can be generalized using the 'map' method on List
  //
  // abstract class List[T] { ...
  //   def filter(p: T => Boolean): List[T] = this match {
  //     case Nil => this
  //     case x :: xs => if(p(x)) x :: xs.filter(p) else xs.filter(p)
  //   }
  //   ...
  // }
  //
  // Using the definition above, posElems can be concisely written as
  def posElems(xs: List[Int]) = xs filter (x => x > 0)
                                                  //> posElems: (xs: List[Int])List[Int]
  posElems(myList)                                //> res5: List[Int] = List(1, 2, 3, 4, 5)

  // Apart from filter there are other methods which supplement it
  //
  // xs filter p is the same as return all elements on which p(x) is true.
  // Predicate p works on x and returns a boolean type.
  myList filter (x => x > 0)                      //> res6: List[Int] = List(1, 2, 3, 4, 5)
  //
  // xs filterNot p = xs filter (x => !p(x))
  // Returns all elements of xs where p(x) is false
  myList filterNot (x => x > 0)                   //> res7: List[Int] = List(-5, -4, -3, -2, -1, 0)
  //
  // xs partition p = (xs filter p, xs filterNot p)
  // Partitions p into a pair, where the first element contains list of elements
  // on which p is satisfied, and the second element contains the list of elements
  // on which p is not satisfied. It is to be noted that the above are computed
  // on a single traversal of a list.
  myList filterNot (x => x > 0)                   //> res8: List[Int] = List(-5, -4, -3, -2, -1, 0)
  //
  // xs takeWhile p: The longest prefix of list xs consisting of elements
  // that all satisfy the predicate p
  myList takeWhile (x => x <= 0)                  //> res9: List[Int] = List(-5, -4, -3, -2, -1, 0)
  //
  // xs dropWhile p: The longest prefix of list xs after any leading elements
  // satsifying p have been removed
  myList dropWhile (x => x <= 0)                  //> res10: List[Int] = List(1, 2, 3, 4, 5)
  // xs span p: Same as (xs takeWhile p, xs dropWhile p), but computed in a single
  // iteration
  myList span (x => x <= 0)                       //> res11: (List[Int], List[Int]) = (List(-5, -4, -3, -2, -1, 0),List(1, 2, 3, 
                                                  //| 4, 5))
  // Another example to clarify things further
  List(2, -4, 5, 7, 1) takeWhile (x => x > 0)     //> res12: List[Int] = List(2)
  List(2, -4, 5, 7, 1) dropWhile (x => x > 0)     //> res13: List[Int] = List(-4, 5, 7, 1)

  // Exercise
  // Define a method pack which does the following
  // pack(List(1, 1, 1, 2, 3, 3, 3, 1)) should give out
  // List(List(1, 1, 1), List(2), List(3, 3), List(1))
  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case x :: xs1 => {
      val (first, rest) = xs span (y => y == x)
      first :: pack(rest)
    }
  }                                               //> pack: [T](xs: List[T])List[List[T]]
  // Testing pack
  pack(List('a', 'a', 'a', 'b', 'c', 'c', 'c', 'a'))
                                                  //> res14: List[List[Char]] = List(List(a, a, a), List(b), List(c, c, c), List(
                                                  //| a))
  // Run length encoding of the list
  // every continuous occurance of elements compressed into (element, no.)
  def encode[T](xs: List[T]): List[(T, Int)] = pack(xs) map (ys => (ys.head, ys.length))
                                                  //> encode: [T](xs: List[T])List[(T, Int)]
  encode(List('a', 'a', 'a', 'b', 'c', 'c', 'c', 'a'))
                                                  //> res15: List[(Char, Int)] = List((a,3), (b,1), (c,3), (a,1))
}