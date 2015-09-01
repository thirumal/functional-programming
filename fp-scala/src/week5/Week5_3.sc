package week5

import math.Ordering

object Week5_3 {
  println("Implicit Parameters")                  //> Implicit Parameters

  // In the last part we saw merge sort,
  // but we wrote the algorithm only to work on List[Int]
  //
  // It makes more sense to actually write it to work with a list of any type
  //
  // But,
  // def msort[T](xs: List[T]): List[T] = ... does not work!
  // It says < in merge is not defined for arbitrary types T
  //
  // This is because we cannot be sure that < is defined for all types!

  // The next best idea is to parameterize < operater along with msort
  // Now we'll be defining it something as follows
  //
  // def msort[T](xs: List[T])(lt: (T, T) => Boolean): List[T] = {
  //   ...
  //     def merge(xs: List[T], ys: List[T]) = (xs, ys) match {
  //       ...
  //       case(x :: xs1, y :: ys1) => if(lt(x, y)) ...
  //       ...
  //     }
  //     val (fst, snd) = xs splitAt n
  //     merge(msort(fst)(lt), msort(snd)(lt))
  //   ...
  //}

  // While calling we'd have to call something like this...
  // msort(List(1,2,...))((x: Int, y: Int) => x < y)

  // But there's already a class in the Scala standard library
  // that represents ordering
  // scala.math.Ordering

  // Instead of parameterizing with lt, it would be better to make it
  // parameterized with Ordering instead
  // So now our definition would be something as follows
  //
  // import math.Ordering
  // ...
  // def msort[T](xs: List[T])(ord: Ordering[T]): List[T] = {
  //   ...
  //     def merge(xs: List[T], ys: List[T]) = (xs, ys) match {
  //       ...
  //       case(x :: xs1, y :: ys1) => if(ord.lt(x, y)) ...
  //       ...
  //     }
  //     val (fst, snd) = xs splitAt n
  //     merge(msort(fst)(ord), msort(snd)(ord))
  //   ...
  //}

  // There are predefined orderings.. like Ordering.Int, Ordering.String
  // we can use those to call our function

  // msort(List(1, 2, ...))(Ordering.Int)

  // Passing these ordering parameters is cumbersome, it would be nice if there
  // was a way for the compiler to magically pick the right ordering (if available).

  def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length
    if(n == 0 || n == 1) {
      xs
    } else {
      // Uses tuple pattern match
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) => if(ord.lt(x, y)) x :: merge(xs1, ys) else y :: merge(xs, ys1)
      }
      // Split the list into two
      val (first, second) = xs splitAt (n / 2)
      merge(msort(first), msort(second))
    }
  }                                               //> msort: [T](xs: List[T])(implicit ord: scala.math.Ordering[T])List[T]

  msort(List(9, 8, 7, 6, 5, 4, 3, 2, 1))          //> res0: List[Int] = List(1, 2, 3, 4, 5, 6, 7, 8, 9)
  msort(List("sugar", "factory", "eats", "banana", "with", "apple"))
                                                  //> res1: List[String] = List(apple, banana, eats, factory, sugar, with)
}