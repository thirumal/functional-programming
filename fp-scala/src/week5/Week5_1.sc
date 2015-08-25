package week5

object Week5_1 {
  println("Week #5, Part #1")                     //> Week #5, Part #1
 
  // More methods useful for list processing
 
  // xs.length --> Number of elements of xs
  // xs.last   --> List's last element, exception if list is empty.
  // xs.init   --> All elements except the last one, exception if list is empty
  // xs take n --> A list containing first n elements of xs, or xs itself is list's length < n
  // xs drop n --> The rest of collection after taking n elements
  // xs (n)    --> Element of xs at index n

  // More utility methods
 
  // Creating new lists
  // xs ++ ys         --> List of all elements of xs followed by all elements of ys (concatenation)
  // xs.reverse       --> List containing all elements of xs in reverse order
  // xs update (n, x) --> Update nth element of xs by x

  // Finding elements
  // xs indexOf x  --> Index of first element in xs equal to x, or -1 if x does not appear in xs
  // xs contains x --> Same as calling xs indexOf x >= 0

  // Implementation of length
  def length[T](xs: List[T]): Int = xs match {
    case List() => 0
    case y :: ys => 1 + length(ys)
  }                                               //> length: [T](xs: List[T])Int
  // Observed Complexity: O(|xs|)
  length(List(1,2,3,4,5))                         //> res0: Int = 5

  // Implementation of last
  def last[T](xs: List[T]): T = xs match {
    case List() => throw new Error("last of empty list");
    case List(x) => x
    case y :: ys => last(ys)
  }                                               //> last: [T](xs: List[T])T
  // Observed Complexity: O(|xs|)
  last(List(1,2,3,4,5))                           //> res1: Int = 5

  // Implementation of init
  def init[T](xs: List[T]): List[T] = xs match {
    case List() => throw new Error("init of empty list");
    case List(x) => Nil
    case y :: ys => y :: init(ys)
  }                                               //> init: [T](xs: List[T])List[T]
  // Observed Complexity: O(|xs|)
  init(List(1,2,3,4,5))                           //> res2: List[Int] = List(1, 2, 3, 4)

  // Implementation of concat (or ++ )
  def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
    case List() => ys
    case z :: zs => z :: concat(zs, ys)
  }                                               //> concat: [T](xs: List[T], ys: List[T])List[T]
  // Observed Complexity: O(|xs|)
  concat(List(1,2,3,4,5), List(6,7,8,9))          //> res3: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9)

  // Implementation of reverse
  def reverse[T](xs: List[T]): List[T] = xs match {
    case List() => Nil
    case y :: ys => reverse(ys) ++ List(y)
  }                                               //> reverse: [T](xs: List[T])List[T]
  // Observed complexity: O(|xs|^2)
  // Can we do better? yes
  reverse(List(1,2,3,4,5))                        //> res4: List[Int] = List(5, 4, 3, 2, 1)

  // Remove n'th element of a list xs. If n is out of bounds, return xs itself
  def removeAt[T](xs: List[T], n: Int): List[T] = xs match {
    case List() => Nil
    case y :: ys => if(n == 0) ys else  y :: removeAt(ys, n - 1)
  }                                               //> removeAt: [T](xs: List[T], n: Int)List[T]
  // Observed complexity: O(|xs|)
  removeAt(List('a', 'b', 'c', 'd'), 1)           //> res5: List[Char] = List(a, c, d)
}