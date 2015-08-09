package week4

object Week4_4 {
  println("Week 4")
  // Some types of lists should be covariant
  // Arrays should not be covariant
  //
  // Lists are immutable
  // Arrays are mutable, as we can update it's elements
  //
  // Immutable types can be covariant if certain conditions
  // are met. Mutable types are not covariant.
  //
  // Say C[T] is a parameterized type and A, B are types
  // such that A <: B.
  // Then there are 3 relationships possible between C[A] and C[B]
  // 1) C[A] <: C[B] // C is covariant
  // 2) C[A] >: C[B] // C is contravariant
  // 3) neither C[A] nor C[B] are subtypes of the other
  //
  // Scala let's you declare the variance of a type by
  // annotating the type parameter
  class C[+A] { ??? } // C is covariant
  class D[-A] { ??? } // D is contravariant
  class E[A]  { ??? } // E is nonvariant (alias invariant)

  // Food for thought
  // type A = IntSet => NonEmpty
  // type B = NonEmpty => IntSet
  //
  // According to Liskov Substitution Principle, which of the
  // following is true?
  //
  // a) A <: B
  // b) B <: A
  // c) A and B are unrelated
  //
  // LSP states that:
  // If A <: B, then everything one can do with a value of
  // type B one should also be able to do with a value of type A
  //
  // A is a true subtype of B (A <: B)
  //
  // Explanation:
  // type B takes a NonEmpty set and returns an IntSet
  // For LSP to hold for A <: B, we ask can you do the same for A?
  // And, yes you can do it:
  // You take a type parameter of NonEmpty (the same type as input
  // for the function having type A and you get back something of
  // type NonEmpty (which is a subtype of IntSet)
  // type A satisfies the same contract of type B, if you give it
  // a NonEmpty set it gives back an IntSet, but type A also does
  // more. Hence A <: B
  
  // Generalizing the rule
  // If we have two function types A1 => B1 and A2 => B2, then
  // A1 => B1 <: A2 => B2
  // only if B1 <: B2 and A2 <: A1
  //
  // This diagram makes it easy to remember this rule
  //
  // A2 => B2
  // ^     |      implies   A1 => B1 <: A2 => B2
  // |     v
  // A1 => B1
  //
  // This implies that functions are contravariant in their
  // argument types and covariant in their result types

  // so now the Function1 trait gets revised to
  // package scala
  trait Function1[-T, +U] {
    def apply(x: T): U
  }

  // When can you annotate a type with + and a - ?
  // Not every type can be annotated with covariance and contravariance
  //
  // If we assume Array to be covariant (with the following definition)

  class CovariantArray[+T] {
    def update(x: T): T = ???
  }

  // The combination of
  // 1) The covariant type parameter T
  // 2) T which appears in the parameter position of method update
  // gave us the headache we saw with Java's Arrays
  //
  // To prevent such blunders Scala compiler takes care of such
  // problematic definitions
  // Roughly, this is what the Scala compiler does
  // 1) covariant type parameters can only appear in method results
  // 2) contravariant type parameters can only appear in method parameters
  // 3) Invariant type can appear anywhere
  //
  // Precise rules are a bit more involved, Scala compiler takes care
  // of it for us.

  // In our previous List definition, we modeled Nil as a class
  // instead of an object. Can that be made an object instead?
  // Yes we can, because we can make List covariant
  //
  // First thing we notice if we change this class to object
  // Objects don't have type parameters, there's only a single
  // instance of them. But removing that will give us the error
  // once we remove the type parameter T, the compiler complains
  // that the type T is not found.
  // We could make Nil extend List[Nothing] as we know that
  // there are no subclasses to Nothing. But when we actually
  // try to instantiate an empty list like below:
  // val x: List[String] = Nil
  // we get errors like List[String] is not compatible with Nil
  // and you may wish to implement List[T] instead.
  //
  // The solution is to make List[T] covariant
  // so change List[T] to List[+T] and things fall into place nicely

  // List trait
  trait List[+T] {
    def isEmpty: Boolean
    def head: T
    def tail: List[T]
    def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)
  }
  // Cons class
  class Cons[T](val head: T, val tail: List[T]) extends List[T] {
    def isEmpty: Boolean = false
  }
  // Nil Class
  object Nil extends List[Nothing] {
    def isEmpty: Boolean = true
    def head: Nothing = throw new NoSuchElementException("Nil.head")
    def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  }
  // Test the implementation
  object Test1 {
    val x: List[String] = Nil
  }

  // Some extra work is usually required to make classes covariant.
  // For example to implement the prepend() method
  //
  // def prepend(elem: T): List[T]
  //
  // an obvious implementation would be something like:
  //
  // def prepend(elem: T): List[T] = new Cons(elem, this)
  //
  // We get covariant type T occurs in contravariant position of type T of value elem
  //
  // In simple words covariant type T present in List[+T] occurs in prepend's parameter elem.
  //
  // We're not mutating states here, we're creating a new object. Why does Scala
  // compiler throw an error in this case?
  //
  // if we allowed the above, this is what would happen.
  //
  // If you have a list called 'xs' of type 'List[IntSet]'
  // you can do the following
  //
  // xs.prepend(Empty)
  //
  // But the same operation on a list of 'ys' of type 'List[NonEmpty]' would
  // lead to a type error
  //
  // ys.prepend(Empty) <= gives 'type mismatch, required: NonEmpty, found: Empty'
  //
  // So, List[NonEmpty] cannot be a subtype of List[IntSet]
  //
  // To correct this problem and can we make the expression variance-correct?
  //
  // Solution: Use a lower-bound
  // def prepend[U >: T](elem: U): List[U] = new Cons(elem, this)
  //
  // with the above change List[NonEmpty] is a subtype of List[IntSet]
  //
  // Exercise:
  // Suppose you have a function 'f' with the following definition
  // def f(xs: List[NonEmpty], x: Empty) = xs prepend x
  // The result would be of type List[IntSet]
  // Why?
  // T is NonEmpty, U could be Empty, but the type inferencer
  // knows that you can return NonEmpty as well, so it promotes
  // U to IntSet and thus you return List[IntSet] instead of List[Empty]
}