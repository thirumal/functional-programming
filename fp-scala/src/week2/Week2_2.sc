package week2

object Week2_2 {
  // Currying

  def cube(x: Int): Int = x * x * x               //> cube: (x: Int)Int
  def fact(x: Int): Int = if (x == 0) 1 else x * fact(x - 1)
                                                  //> fact: (x: Int)Int

  // rewrite the sum function to return a function
  // input: a function f which takes an integer argument, returns an integer
  // output: a function g which takes two integers as argument, returns
  // the sigma(a,b) of f(n)
  def sum1(f: Int => Int): (Int, Int) => Int = {
    def sumF(a: Int, b: Int): Int = {
      if (a > b) 0 else f(a) + sumF(a + 1, b)
    }
    sumF
  }                                               //> sum1: (f: Int => Int)(Int, Int) => Int
  // so now we can define our example functions in the following format
  def sumInts4 = sum1(x => x)                     //> sumInts4: => (Int, Int) => Int
  def sumCubes4 = sum1(x => x * x * x)            //> sumCubes4: => (Int, Int) => Int
  def sumFacts4 = sum1(fact)                      //> sumFacts4: => (Int, Int) => Int

  // and now these can be called as
  sumCubes4(1, 5)                                 //> res0: Int = 225
  // but a smarter way to call this would be:
  sum1(cube)(1, 5)                                //> res1: Int = 225

  // to note:
  (sum1(cube))(1, 5) == sum1(cube)(1, 5)          //> res2: Boolean = true

  // there's a special syntax to accomodate functions returning functions
  // the above function sum1 can be rewritten as
  def sum2(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 0 else f(a) + sum2(f)(a + 1, b)
  }                                               //> sum2: (f: Int => Int)(a: Int, b: Int)Int

  // note note:
  val fn1 = sum1(cube)                            //> fn1  : (Int, Int) => Int = <function2>
  // but you cannot write
  // val fn = sum2(cube) // error!!!

  // let's analyze the type of sum()
  // (Int => Int) => ( (Int, Int) => Int )
  // can also be written as
  // (Int => Int) => (Int, Int) => Int
  // so this means that function types associate to the right!
  // Int => Int => Int is equivalent to Int => (Int => Int)

  // Exercise: Write product_of_interval(a, b)
  def product1(f: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1 else f(a) * product1(f)(a + 1, b)
  }                                               //> product1: (f: Int => Int)(a: Int, b: Int)Int

  // write factorial in terms of product
  // so factorial of 6 is nothing but
  def fact1(n: Int): Int = product1(x => x)(1, n) //> fact1: (n: Int)Int
  fact1(6)                                        //> res3: Int = 720


  // can we generalize both sum and product to be represnted by something else?

  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
    if(a > b) zero else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
  }                                               //> mapReduce: (f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b
                                                  //| : Int)Int

  // iterative version of mapreduce
  def mapReduce1(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int = {
    def iter(acc: Int, a: Int): Int = {
      if (a > b) acc
      else iter(combine(f(a), acc), a + 1)
    }
    iter(zero, a)
  }                                               //> mapReduce1: (f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int,
                                                  //| b: Int)Int


  // sum can be written by
  def sum(f: Int => Int)(a: Int, b: Int) = mapReduce1(f, (x, y) => x + y, 0)(a, b)
                                                  //> sum: (f: Int => Int)(a: Int, b: Int)Int
  def product(f: Int => Int)(a: Int, b: Int) = mapReduce1(f, (x, y) => x * y, 1)(a, b)
                                                  //> product: (f: Int => Int)(a: Int, b: Int)Int

  // testing our implementation
  // sum of squares of 1 to 3
  sum(x => x * x)(1, 3)                           //> res4: Int = 14
  // factorial of 6
  def fact2(n: Int): Int = product(x => x)(1, n)  //> fact2: (n: Int)Int
  fact2(6)                                        //> res5: Int = 720

}
