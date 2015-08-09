package week4

object Week4 {
  // Part 1:
  // In a pure object oriented oriented language every value is an object
  // and every operation is some method call on the object.
  //
  // This further implies that every type of each value is a class
  // (as every value is an object)
  //
  // Conceptually types such as Int, MyBoolean don't receive any special treatment
  // they can be treated as subclasses of the superclass AnyVal
  // (defined in package scala)
  //
  // For the reasons of efficiency, in the end scala compiler represents
  // the type scala.Int as 32-bit integer and scala.MyBoolean as Java's MyBoolean
  // etc. (it is an optimization, improve interoperability between Scala and
  // java code, conceptually types are still classes and values are objects)
  //
  // Here's how we might implement our MyBoolean class

  abstract class MyBoolean {
    // instead of if (cond) { thenPart } else { elsePart }
    // it would be translated to cond.ifThenElse(thenPart, elsePart)
    def ifThenElse[T](thenPart: => T, elsePart: => T): T

    // The && operator
    // Table:
    // f && f = f
    // f && t = f
    // t && f = f
    // t && t = t
    def && (x: => MyBoolean): MyBoolean = ifThenElse(x, False)
    // The || operator
    // Table:
    // f || f = f
    // f || t = t
    // t || f = t
    // t || t = t
    def || (x: => MyBoolean): MyBoolean = ifThenElse(True, x)
    // The ! operator
    // Table:
    // !f = t
    // !t = f
    def unary_! : MyBoolean = ifThenElse(False, True)
    // The == operator
    // Table:
    // f == f = t
    // f == t = f
    // t == f = f
    // t == t = t
    def == (x: MyBoolean): MyBoolean = ifThenElse(x, !x)
    // The != operator
    // Table:
    // f != f = f
    // f != t = t
    // t != f = t
    // t != t = f
    def != (x: MyBoolean): MyBoolean = ifThenElse(!x, x)

    // Exercise: implement '<' operator
    // assuming false < true
    // Table:
    // f < f = f
    // f < t = t
    // t < f = f
    // t < t = f
    def < (x: MyBoolean): MyBoolean = ifThenElse(False, x)
    // ...
  }

  // Now define the true object
  object True extends MyBoolean {
    def ifThenElse[T](thenPart: => T, elsePart: => T): T = thenPart
  }

  // Now define the false object
  object False extends MyBoolean {
    def ifThenElse[T](thenPart: => T, elsePart: => T): T = elsePart
  }

  // Our class for Int would be something like this
  abstract class MyInt {
    // Arithmetic operations
    def + (that: Double): Double
    def + (that: Float): Float
    def + (that: Long): Long
    def + (that: Int): Int
    // ... similarly operations for -, *, / and % are defined

    // Shift operations
    def << (cnt: Int): Int
    // ... similarly operations for >>, >>> are defined

    // Logical operations
    def & (that: Long): Long
    def & (that: Int): Int
    // ... similarly operations for |, ^ are defined

    // Comparision operations
    def == (that: Double): MyBoolean
    def == (that: Float): MyBoolean
    def == (that: Long): MyBoolean
    def == (that: Int): MyBoolean
    // .. similarly operations for !=, <, >, <=, >= are defined
  }

  // Exercise 2: Implement the natural numbers class
  // Peano numbers
  abstract class Nat {
    def isZero: Boolean
    def predecessor: Nat
    def successor = new Succ(this)
    def + (that: Nat): Nat
    def - (that: Nat): Nat
  }
  // Object to represent zero
  object Zero extends Nat {
    def isZero = true
    def predecessor: Nothing = throw new NoSuchElementException("Zero has no predecessor")
    def + (that: Nat): Nat = that
    def - (that: Nat): Nat = {
      if(that.isZero) this
      else throw new NoSuchElementException("Any non-negative number cannot be subtracted from zero")
    }
  }

  // Class to represent the successor of a number
  class Succ(n: Nat) extends Nat {
    def isZero = false
    def predecessor = n
    def + (that: Nat) = new Succ(n + that)
    def - (that: Nat) = if (that.isZero) this else n - that.predecessor
  }

  // Part 2:
  // IMPORTANT: Before starting the session what you should know about
  // how apply() works. Here's a simple example
  // even though it is explained later on, with this thought
  // you can understand the rest of the lecture more clearly!
  class Square {
    def apply(x: Int) = x * x
  }
  val square = new Square                         //> square  : week4.Week4.Square = week4.Week4$Square@51521cc1
  square(5)                                       //> res0: Int = 25
  square(10)                                      //> res1: Int = 100

  // Function types relate to classes and how function values relate to objects

  // Are functions objects? Yes

  // Function objects are treated as objects in Scala
  //
  // therefore the type A => B is an abbreviation for
  // the class scala.Function1[A, B]
  //
  // package scala
  trait Function1[A, B] {
    def apply(x: A): B
  }

  // Similarly you have (A, B) => C defined as Function2 which might
  // be defined as follows
  // package scala
  trait Function2[A, B, C] {
    def apply(x: A, y: B): C
  }
  // Currently there's till Function22 (lecture, don't know if it has changed)

  // We looked at function types, lets look at function values

  // So if we're defining a anonymous function like below
  // notice the type <function1>
  (x: Int) => x * x                               //> res2: Int => Int = <function1>

  // in reality it is just a Anonymous class with the following
  // definition and we return that object to be used
  {
    class AnonFun extends Function1[Int, Int] {
      def apply(x: Int) = x * x
    }
    new AnonFun
  }                                               //> res3: AnonFun = week4.Week4$$anonfun$main$1$AnonFun$1@39ba5a14

  // there's a shorter syntax for the above: Anonymous class syntax
  new Function1[Int, Int] {
    def apply(x: Int) = x * x
  }                                               //> res4: week4.Week4.Function1[Int,Int] = week4.Week4$$anonfun$main$1$$anon$1@
                                                  //| 511baa65

  // So every function defined in Scala is just an object which extends the
  // FunctionN trait with an apply method()
  //
  // So we know that f.apply(a,b) === f(a,b)
  //
  // apply itself is not an object.. or else we'd end up in an infinte loop of expansion
  //
  // Note that the definition of the method
  // def f(x: Int): MyBoolean = ...
  // by itself is not a function value (hence an object)
  //
  // only when f is used where a FunctionX type is expected, it is
  // automatically converted to a function value
  // like
  //
  // (x: Int) => f(x)
  //
  // or in other words it is expanded as
  //
  // new Function1[Int, MyBoolean] {
  //   def apply(x: Int) = f(x)
  // }
  //
  // A.K.A Eta-expansion in lambda calculus

  // In-lecture exercise
	// Trait for list
	trait List[T] {
	  def isEmpty: Boolean
	  def head: T
	  def tail: List[T]
	}
	// Cons class
	class Cons[T](val head: T, val tail: List[T]) extends List[T] {
	  def isEmpty = false
	}
	// Nil Class
	class Nil[T] extends List[T] {
	  def isEmpty = true
	  def head: Nothing = throw new NoSuchElementException("Nil.head")
	  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
	}
	// Define an object List, which you can instantiate using
	// List(), List(1) and List(1,2) etc.
	object List {
	  // w.k.t List(1, 2) is actually List.apply(1, 2)
	  def apply[T](): List[T] = new Nil
	  def apply[T](x1: T): List[T] = new Cons(x1, new Nil)
	  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, new Nil))
	}
	// Test the implementation
	List()                                    //> res5: week4.Week4.List[Nothing] = week4.Week4$$anonfun$main$1$Nil$1@340f438
                                                  //| e
	List(1)                                   //> res6: week4.Week4.List[Int] = week4.Week4$$anonfun$main$1$Cons$1@30c7da1e
	List(1,2)                                 //> res7: week4.Week4.List[Int] = week4.Week4$$anonfun$main$1$Cons$1@5b464ce8
}
