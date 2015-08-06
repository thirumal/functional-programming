package week4

object Week4 {

  // IMPORTANT: Before starting the session what you should know about
  // how apply() works. Here's a simple example
  // even though it is explained later on, with this thought
  // you can understand the rest of the lecture more clearly!
  class Square {
    def apply(x: Int) = x * x
  }
  val square = new Square                         //> square  : Week5.Square = Week5$Square@2957fcb0
  square(5)                                       //> res0: Int = 25
  square(10)                                      //> res1: Int = 100

  // Function types relate to classes and how function values relate to objects
  
  // Are functions objects? Yes
  
  // Function objects are treated as objects in Scala
  
  // therefore the type A => B is an abbreviation for
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
  }                                               //> res3: AnonFun = Week5$$anonfun$main$1$AnonFun$1@7f63425a

  // there's a shorter syntax for this: Anonymous class syntax
  new Function1[Int, Int] {
    def apply(x: Int) = x * x
  }                                               //> res4: Week5.Function1[Int,Int] = Week5$$anonfun$main$1$$anon$1@36d64342
  
  // So every function defined in Scala is just an object which extends the
  // FunctionN trait with an apply method()
  
  // So we know that f.apply(a,b) === f(a,b)

  // apply itself is not an object.. or else we'd end up in an infinte loop of expansion
  
  // the expansion from
  // def f(x: Int): Boolean = ???
  // to
  // is expanded as
  // new Function1[Int, Boolean] {
  //   def apply(x: Int) = f(x)
  // }
  // A.K.A Eta-expansion in lambda calculus
  
  
}
