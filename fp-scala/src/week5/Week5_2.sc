package week5

object Week5_2 {
  println("Week 5, #2 - Pairs and tuples")        //> Week 5, #2 - Pairs and tuples
  // Define a function to sort lists that is
  // more efficient than insertion sort.
  // A good sorting algorithm for functional lists
  // is merge sort.
  //
  // Base Case: If a list contains 0 or 1 element
  // it is already sorted (trival)
  //
  // Otherwise,
  // 1) Seperate the list into two sub-lists,
  // each containing around half of the elements
  // of the original list
  // 2) Sort the two sub-lists
  // 3) Merge the two sorted sub-lists into one single
  // sorted list.

  // mySplitAt implementation?
  def mySplitAt(xs: List[Int], n: Int): (List[Int], List[Int]) = {
    def splitAtInner(acc: List[Int], left: List[Int], n: Int): (List[Int], List[Int]) = {
      if(n == 0) {
        (acc.reverse, left)
      } else {
        splitAtInner(left.head :: acc, left.tail, n - 1)
      }
    }
    splitAtInner(List(), xs, n)
  }                                               //> mySplitAt: (xs: List[Int], n: Int)(List[Int], List[Int])
  // Tests for mySplitAt
  List(1,2,3,4,5) splitAt 0                       //> res0: (List[Int], List[Int]) = (List(),List(1, 2, 3, 4, 5))
  mySplitAt(List(1,2,3,4,5), 0)                   //> res1: (List[Int], List[Int]) = (List(),List(1, 2, 3, 4, 5))
  List(1,2,3,4,5) splitAt 1                       //> res2: (List[Int], List[Int]) = (List(1),List(2, 3, 4, 5))
  mySplitAt(List(1,2,3,4,5), 1)                   //> res3: (List[Int], List[Int]) = (List(1),List(2, 3, 4, 5))
  List(1,2,3,4,5) splitAt 2                       //> res4: (List[Int], List[Int]) = (List(1, 2),List(3, 4, 5))
  mySplitAt(List(1,2,3,4,5), 2)                   //> res5: (List[Int], List[Int]) = (List(1, 2),List(3, 4, 5))
  List(1,2,3,4,5) splitAt 3                       //> res6: (List[Int], List[Int]) = (List(1, 2, 3),List(4, 5))
  mySplitAt(List(1,2,3,4,5), 3)                   //> res7: (List[Int], List[Int]) = (List(1, 2, 3),List(4, 5))
  List(1,2,3,4,5) splitAt 4                       //> res8: (List[Int], List[Int]) = (List(1, 2, 3, 4),List(5))
  mySplitAt(List(1,2,3,4,5), 4)                   //> res9: (List[Int], List[Int]) = (List(1, 2, 3, 4),List(5))
  List(1,2,3,4,5) splitAt 5                       //> res10: (List[Int], List[Int]) = (List(1, 2, 3, 4, 5),List())
  mySplitAt(List(1,2,3,4,5), 5)                   //> res11: (List[Int], List[Int]) = (List(1, 2, 3, 4, 5),List())

  // Merge - Primitive routine. Attempt #1
  // Does not look so cool...
  // Uses pattern matching
  def merge1(xs: List[Int], ys: List[Int]): List[Int] = xs match {
    case Nil => ys
    case x :: xs1 => {
      ys match {
        case Nil => xs
        case y :: ys1 => {
          if (x < y) x :: merge1(xs1, ys) else y :: merge1(xs, ys1)
        }
      }
    }
  }                                               //> merge1: (xs: List[Int], ys: List[Int])List[Int]

  // Pairs
  val myPair = ("answer", 42)                     //> myPair  : (String, Int) = (answer,42)
  println("Pair: " + myPair)                      //> Pair: (answer,42)
  val (myLabel, myValue) = myPair                 //> myLabel  : String = answer
                                                  //| myValue  : Int = 42
  println("Label: " + myLabel)                    //> Label: answer
  println("Value: " + myValue)                    //> Value: 42

  // Tuples - Same like pairs, but more than two elements
  val myTuple = ("first", "second", 3)            //> myTuple  : (String, String, Int) = (first,second,3)
  println("Tuple: " + myTuple)                    //> Tuple: (first,second,3)
  val (my1st, my2nd, my3rd) = myTuple             //> my1st  : String = first
                                                  //| my2nd  : String = second
                                                  //| my3rd  : Int = 3
  // val (my1st, my2nd, my3rd, my4th) = myTuple // DOES NOT WORK!
  // val (my1st1, my2nd1) = myTuple // DOES NOT WORK AS WELL!

  // Definition of Tuple2 would be something of follows
  case class Tuple2[+T1, +T2](_1: T1, _2: T2) {
    override def toString = "(" + _1 + "," + _2 + ")"
  }
  // This also shows that fields of a tuple can be accessed using _n
  
  // So the pattern match  'val (label, value) = pair'
  // is equivalent to
  // val label = pair._1
  // val value = pair._2

// Merge Sort wrapper routine
  def mergeSort(xs: List[Int]): List[Int] = {
    val n = xs.length
    if(n == 0 || n == 1) {
      xs
    } else {
      // Uses tuple pattern match
      def merge(xs: List[Int], ys: List[Int]): List[Int] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) => if(x < y) x :: merge(xs1, ys) else y :: merge(xs, ys1)
      }
      // Split the list into two
      val (first, second) = xs splitAt (n / 2)
      merge(mergeSort(first), mergeSort(second))
    }
  }                                               //> mergeSort: (xs: List[Int])List[Int]
  // Test merge sort implementation
  mergeSort((1 to 10).toList)                     //> res12: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  mergeSort(List(10, 9, 8, 7, 6, 5, 4, 3, 2, 1))  //> res13: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

}