package week3

object Week3_2 {
  println("Week3 - Part 2, Polymorphism")         //> Week3 - Part 2, Polymorphism
  /*

  This chapter is about polymorphism

  A cons-list (think of an immutable linked list) is made up of
  1) Nil = an empty list
  2) Cons = a cell containing remainder of the list

  */

  trait List[T] {
    def isEmpty: Boolean
    def head: T
    def tail: List[T]
  }

  /*
  value parameters act as fields or methods inside
  the class, you can also use a value parameter
  to implement abstract fields and methods
  */

  class Cons[T](val head: T, val tail: List[T]) extends List[T] {
    def isEmpty: Boolean  = false
  }

  class Nil[T] extends List[T] {
    def isEmpty: Boolean = true
    def head: Nothing = throw new NoSuchElementException("Nil.head")
    def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  }

  // type parameters are also valid for functions
  def singleton[T](elem: T) = {
    new Cons[T](elem, new Nil[T])
  }                                               //> singleton: [T](elem: T)week3.Week3_2.Cons[T]

  // you can create singleton elements like below
  singleton[Int](1)                               //> res0: week3.Week3_2.Cons[Int] = week3.Week3_2$$anonfun$main$1$Cons$1@1e643f
                                                  //| af
  singleton[Boolean](true)                        //> res1: week3.Week3_2.Cons[Boolean] = week3.Week3_2$$anonfun$main$1$Cons$1@6e
                                                  //| 8dacdf
  // type can also be inferred automatically
  // by the compiler
  singleton(2)                                    //> res2: week3.Week3_2.Cons[Int] = week3.Week3_2$$anonfun$main$1$Cons$1@7a79be
                                                  //| 86

  // all type parameters and arguments are
  // removed before evaluation
  // this is known as `type erasure`
  // including Scala

  // so now we can conclude that there are two
  // types of polymorphism
  // 1) subtyping: Instances of subclass can
  //    be passed to a base class
  //
  // 2) generics

  // Implement the nth element in a list method
  // should throw out of bounds exception
  // if not in range [0, n)
  def nthElement[T](list: List[T], n: Int): T = {
    if (list.isEmpty) throw new ArrayIndexOutOfBoundsException("List is empty")
    else if (n == 0) list.head
    else nthElement(list.tail, n - 1)
  }                                               //> nthElement: [T](list: week3.Week3_2.List[T], n: Int)T

}