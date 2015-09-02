package week6

object Week6_2 {
  println("Combinatorial Search and For-Expressions")
                                                  //> Combinatorial Search and For-Expressions
  // Higher order functions and collections replace
  // nested loop from imperitive languages.
  // Programs often using nested loops can be expressed
  // using a combination of higher order functions.

  // For example given a positive integer n. Find all pairs
  // of integers i, j with 1 <= j < i < n such that
  // i + j is prime.
  // Helper routine: A method to tell whether a routine is prime
  def isPrime(x: Int) = (2 until x) forall (x % _ != 0)
                                                  //> isPrime: (x: Int)Boolean
  //
  def sumOfPrimes1(n: Int) = {
    (1 until n) flatMap (i =>
      (1 to i) map (j => (i, j))) filter { case (x, y) => isPrime(x + y) }
  }                                               //> sumOfPrimes1: (n: Int)scala.collection.immutable.IndexedSeq[(Int, Int)]
  sumOfPrimes1(7)                                 //> res0: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((1,1), (2,1
                                                  //| ), (3,2), (4,1), (4,3), (5,2), (6,1), (6,5))

  // The above routine is concise and very powerful.
  // But if you read this code above in a month you'd not be able to
  // understand it without breaking your head again!

  // Is there a simpler way by which such expressions could be expressed
  // in a more easier and concise way?

  // This is where Scala's for expressions come handy.

  // Let's say we have a person calss like below
  case class Person(name: String, age: Int)
  val persons = List(Person("a", 10), Person("b", 25), Person("c", 30))
                                                  //> persons  : List[week6.Week6_2.Person] = List(Person(a,10), Person(b,25), Pe
                                                  //| rson(c,30))
  // To obtain names of people over 20 years old, we'd use:
  for (p <- persons if p.age > 20) yield p.name   //> res1: List[String] = List(b, c)
  // The above is equivalent to
  persons filter (p => p.age > 20) map (p => p.name)
                                                  //> res2: List[String] = List(b, c)

  // The for expressions is similar to loops in imperitive languages, except
  // that it builds a list of the results of all iterations.

  // A for loop usually causes some side effect. A for expression produces
  // a new result (list of p.names satisfying the condition in our case).

  // Full syntax of for expression
  // for ( s ) yield e.
  // s: Sequence of generators and filters
  // e: expression whose value is returned by an iteration.
  //
  // A generator is in the form of 'p <- e', where p is a pattern and e is an
  // expression whose value is a collection.
  //
  // A filter is of the form 'if f', where f is boolean expression.
  //
  // The sequence s must start with a generator. If there are several generators
  // then the last generators varies faster than the first.
  //
  // Instead of ( s ), { s } can be used. Note that ( s ) is a simple expression.
  // And { s } is a complex expression, and when used we have no need to add a ;
  // at the end of each statement/expression.

  // Rewriting our example
  def sumOfPrimes(n: Int) = {
    for {
      i <- 1 until n
      j <- 1 until i
      if isPrime(i + j)
    } yield (i, j)
  }                                               //> sumOfPrimes: (n: Int)scala.collection.immutable.IndexedSeq[(Int, Int)]
  sumOfPrimes(7)                                  //> res3: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,1), (3,
                                                  //| 2), (4,1), (4,3), (5,2), (6,1), (6,5))

  def scalarProduct(xs: List[Double], ys: List[Double]): Double = {
    (for ((x,y) <- xs zip ys) yield x * y).sum
  }                                               //> scalarProduct: (xs: List[Double], ys: List[Double])Double
  scalarProduct(List(1.0, 2.0, 3.0), List(2.0, 4.0, 6.0))
                                                  //> res4: Double = 28.0
}