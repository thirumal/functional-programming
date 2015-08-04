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
  }

  // you can create singleton elements like below
  singleton[Int](1)
  singleton[Boolean](true)
  // type can also be inferred automatically
  // by the compiler
  singleton(2)

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
  }

}