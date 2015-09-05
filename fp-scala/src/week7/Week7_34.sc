package week7

object Week7_34 {
  println("Lazy Evaluation")                      //> Lazy Evaluation
  // Scala uses strict evaluation (as other normal programming languages) by default
  // But lazy evaluation is very attractive, because we can be sure
  // that a functional method does not yield any side-effects.

  // Declaring a lazy variable
  // lazy val x = <expression>

  // Seeing lazy evaluation in action
  def expression = {
    val x = { println("x"); 1 }
    lazy val y = { println("y"); 2 }
    def z = { println("z"); 3 }
    z + y + x + z + y + x
  }                                               //> expression: => Int
  // Evaluate the expression
  expression                                      //> x
                                                  //| z
                                                  //| y
                                                  //| z
                                                  //| res0: Int = 12
  // Let's analyze why it printed xzyz
  // As soon as we encounter a val we evaluate it and store it
  // def and lazy val are not evaluted until they are encountered
  // Once encountered lazy val's info is stored. But def's value is
  // discared. When called again lazy val looks up the stored information
  // but def keeps getting evaluated.

  // Now the Stream can be implemented even more efficiently!
  //   ...
  //   def cons[T](hd: T, tl: => Stream[T]) = new Stream[T] {
  //     def isEmpty = false
  //     def head = hd
  //     lazy val tail = t
  //   }
  //   ...
  //
  // So now we'll evalute tail only when needed, but once evaluated
  // we avoid the neccessity for repeated computation again and again.

  // NEXT SECTION:
  // This laziness can be exploited when working with infinite quantities.
  // We evaluate only when we're in the need of evaluating it.

  // This opens up the possiblity of defining infinite streams
  // So we could define the stream of infinite natural numbers
  // starting from n as follows
  def from(n: Int): Stream[Int] = n #:: from (n + 1)
                                                  //> from: (n: Int)Stream[Int]
  // Now we can say that the stream of all natural numbers would be
  val nats = from(0)                              //> nats  : Stream[Int] = Stream(0, ?)
  nats(0)                                         //> res1: Int = 0
  nats(10)                                        //> res2: Int = 10
  // Stream of all multiples of four would be
  val mul4s = nats map (_ * 4)                    //> mul4s  : scala.collection.immutable.Stream[Int] = Stream(0, ?)
  // test mul4s
  (mul4s take 10).toList                          //> res3: List[Int] = List(0, 4, 8, 12, 16, 20, 24, 28, 32, 36)

  // One place where we can put this principle in good use is calculation
  // of prime numbers. There's a very old algorithm called the seive of
  // eratosthenes.
  // The basic idea is:
  // 1) Start with all integers from 2, the first prime number
  // 2) Eliminitate all multiples of 2
  // 3) The first element of the resulting list is 3, a prime number
  // 4) Eliminiate all multiples of 3
  // Iterate forever. At each step, the first number in the list
  // is a prime number and we eliminate all its multiples
  def seive(s: Stream[Int]): Stream[Int] = {
    s.head #:: seive(s.tail filter (_ % s.head != 0))
  }                                               //> seive: (s: Stream[Int])Stream[Int]
  // Now get the first 10 prime numbers
  (seive(from(2)) take 10).toList                 //> res4: List[Int] = List(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)

  // Square root function can also be modified accordingly
  // without having to worry about when to end
  def sqrtStream(d: Double): Stream[Double] = {
    def improve(g: Double): Double = (g + d / g) / 2
    // return a stream of iterations
    lazy val guesses: Stream[Double] = 1 #:: (guesses map improve)
    guesses
  }                                               //> sqrtStream: (d: Double)Stream[Double]
  // Get the value of the square root after 10 iterations
  sqrtStream(4)(10)                               //> res5: Double = 2.0

  // We have decoupled the termination criteria from the computation routine
  // Ofcourse, we can add the termination critera later on.
  val DELTA = 0.0000000000000000001               //> DELTA  : Double = 1.0E-19
  def isGoodEnough(guess: Double, x: Double) = math.abs(guess *guess - x) < DELTA
                                                  //> isGoodEnough: (guess: Double, x: Double)Boolean

  // You can apply this as follows, get the first iteration where the guess is good enough.
  sqrtStream(4).filter(isGoodEnough(_, 4))(0)     //> res6: Double = 2.0
}