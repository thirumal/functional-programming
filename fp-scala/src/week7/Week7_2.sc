package week7

object Week7_2 {
  println("Streams")                              //> Streams
  // Streams are similar to lists, but their tail is evaluated only on demand

  // A very good use case would be:
  // Find the second prime number between 10,000 and 100,000
  // For brevity let's consider the range 1 to 10 below.

  // Helper prime identifer routine
  // IGNORE toStream for now...
  def isPrime(n: Int) = (2 until n).toStream forall (_ % n != 0)
                                                  //> isPrime: (n: Int)Boolean

  // Easier to write, but very inefficient.
  // We first check whether each number whether it's prime
  // and then once filtered we select the second element
  ((2 to 10) filter isPrime)(1)                   //> res0: Int = 3

  // The recursive, but efficient alternative
  // Too long for my taste!
  def nthPrime(from: Int, to: Int, n: Int): Int = {
    if(from >= to) {
      throw new Error("No prime present")
    } else if(isPrime(from)) {
      if(n == 1) from
      else nthPrime(from + 1, to, n - 1)
    } else {
      nthPrime(from + 1, to, n)
    }
  }                                               //> nthPrime: (from: Int, to: Int, n: Int)Int
  def secondPrime(from: Int, to: Int) = nthPrime(from, to, 2)
                                                  //> secondPrime: (from: Int, to: Int)Int
  //Test
  secondPrime(2, 10)                              //> res1: Int = 3

  // There's a trick that can make the short code efficient as well..
  // The core idea is to never evaluate the tail of the sequence
  // unless absolutely needed. This idea is implemented in a new class
  // called 'Stream'

  // How you define stream
  val xs = Stream.cons(1, Stream.cons(2, Stream.empty))
                                                  //> xs  : Stream.Cons[Int] = Stream(1, ?)
  // Can also define it as a factory
  Stream(1, 2, 3)                                 //> res2: scala.collection.immutable.Stream[Int] = Stream(1, ?)

  // You can convert a collection into a stream
  (1 to 1000).toStream                            //> res3: scala.collection.immutable.Stream[Int] = Stream(1, ?)

  // Notice that second element onwards is not evaluated (?)

  // Stream Ranges (How a stream is evaluated)
  def streamRange(lo: Int, hi: Int): Stream[Int] = {
    if (lo >= hi) Stream.empty
    else Stream.cons(lo, streamRange(lo + 1, hi))
  }                                               //> streamRange: (lo: Int, hi: Int)Stream[Int]
  // Compare this with the a similar function that produces a list
  def listRange(lo: Int, hi: Int): List[Int] = {
    if (lo >= hi) Nil
    else lo :: listRange(lo + 1, hi)
  }                                               //> listRange: (lo: Int, hi: Int)List[Int]
  // listRange evaluates completely and gives back a list, period.
  // But streamRange will only evaluate till the first Stream.cons.
  // It places the value lo in the con's first place and the second place
  // will contain an object that would know how to evaluate further
  // and return this special cons pair.
  // As and when you keep calling tail recursively, we evaluate this
  // one after another. Till the tail is not called, it is not evaluated.

  // So our prime number example can be written as
  ((2 to 10).toStream filter isPrime)(1)          //> res4: Int = 3

  // You can do almost all operations of lists on Streams.
  // The one exception is the cons operation ::
  // But we do have a special operator #:: that produces
  // a stream. To be noted that this operator can be used
  // in both expressions as well as patterns.
  // x #:: xs <===> Stream.cons(x, xs)

  // Implementation of Streams is quite close to the one of lists.
  // trait Stream[+A] extends Seq[A] {
  //   def isEmpty: Boolean
  //   def head: A
  //   def tail: Stream[A]
  //   ...
  // }
  // As with lists, all other methods can be defined in terms of these three above.
  // The concrete implementation is as follows
  // object Stream {
  //   // !!! NOTICE the call by name evaluation of tail !!!
  //   def cons[T](hd: T, tl: => Stream[T]) = new Stream[T] {
  //     def isEmpty = false
  //     def head = hd
  //     def tail = tl
  //   }
  //   def empty = new Stream[Nothing] {
  //     def isEmpty = true;
  //     def head = throw new Error("empty.head")
  //     def tail = throw new Error("empty.tail")
  //   }
  // }
}