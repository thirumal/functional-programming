package week4

object Week4_3 {
  println("Week 3")                               //> Week 3
  // Part 3:
  //
  // Two types of polymorphism
  //
  // 1) Subtyping: where we can pass instances of a subtype where
  //    instances of base type were required
  //
  // 2) Generics: Parameterizing types with other types

  // Interactions between the two
  // 1) Bounds (subject type parameters to subtype constraints)
  // 2) Variance (how parameterized type behave under subtyping)
  
  // Type bounds:
  // Consider an example function assertAllPos which
  // returns IntSet if all elements in the set are positive
  // or throws an exception if not the case.
  // While implementing we'd seperate the defintions between
  // the Empty and the NonEmpty case
  //
  // assertAllPos(Empty) = Empty
  // assertAllPos(NonEmpty(...)) = {NonEmpty(...), Exception}
  //
  // this knowledge of Empty passed to assertAllPos returns Empty
  // and NonEmpty passed to assertAllPos returns NonEmpty is lost
  // when we define assertAllPos as follows
  // def assertAllPos(s: IntSet): IntSet
  //
  // Therefore we express this with Bounds as follows
  // def assertAllPos[S <: IntSet](r: S): S = ???
  // "<:" means IntSet is an upper bound of the type parameter S
  //
  // Generally the notation
  //
  // S <: T means S is a subtype of T or T is a supertype of S
  // S >: T means S is a supertype of T or T is a subytpe of S
  //
  // While drawing the flow chart of S <: T we represent it as
  // T
  // ^
  // |
  // S
  // arrow from subtype to supertype
  //
  // For Example specifying [S >: NonEmpty] means that
  // S can be one of NonEmpty, IntSet, AnyRef or Any
  //
  // Mixing lower bound and upper bound is also allowed
  // [S >: NonEmpty <: IntSet] means that S can be one of
  // either NonEmpty or IntSet
  //
  // We know that NonEmpty <: IntSet
  // What if we wrap both the above with a List?
  // Should List[NonEmpty] <: List[IntSet] ?
  //
  // Intuitively this makes sense. A list of non-empty sets
  // is a special case of lists of arbitrary types
  //
  // For all the types where this relationship holds we call them
  // 'covariant', because their subtyping relationship varies
  // exactly like the type parameter.
  // So List type is covariant. Are all types covariant?
  //
  // Array in Java is covariant, so if we have an array of type T
  // then T[] is considered covariant
  //
  // Therefore NonEmpty[] <: IntSet[]
  //
  // So it follows that
  // NonEmpty[] a = new NonEmpty[]{ new NonEmpty(1, Empty, Empty)};
  // IntSet[] b = a;
  // b[0] = Empty;
  // NonEmpty s = a[0]; // culprit!
  //
  // We were able to assign an Empty set instance to a variable
  // of type NonEmpty. This is completely wrong
  //
  // To counteract this problem, Java stores a type tag along with
  // every array which contains the type of the original array
  // (mind you the tag is with the memory allocated for the array
  // and not the references). This allows Java to raise a
  // ArrayStoreException which such situations arise.
  //
  // When you look at it, this should actually be a compile time
  // check, but instead we've made this a compile time error.
  // Also, on every assignment a type check needs to be performed
  // adding performance overhead to Array assignments.
  //
  // Why did the designers of Java do this?
  //
  // Initially during the Java 1.0 days we did not have generics.
  // To counteract this and to adhere to the DRY principle, the
  // designers wanted to be able to have a method like
  //
  // sort(Object[] a) // able to sort any kind of Object
  // But ultimately, this plan backfired badly!

  // So when can a type be a subtype of another?
  // The following principle has to be kept in mind
  //
  // If A <: B, then everything one can do with a value of
  // type B one should also be able to do with a value of type A
  //
  // This was first laid out by Barbara Liskov, and hence
  // The Liskov Substitution Principle.
  
  // NOTE: Arrays are just specializations of functions in Scala.
  // TODO: Explore this topic!
}