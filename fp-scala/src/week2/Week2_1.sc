package week2

object Week2_1 {
  // Higher order functions
  // lets you pass functions as arguments, return them as well.
  // such values are called first-class values
  
  // first order functions only act on data, not functions
  
  // consider the following three examples
  
  // Sum of all integers in the interval [a, b]
  def id(x: Int): Int = x                         //> id: (x: Int)Int
  def sumInts(a: Int, b: Int): Int = {
    if (a > b) 0 else a + sumInts(a + 1, b)
  }                                               //> sumInts: (a: Int, b: Int)Int
  
  // Sum of all cubes of all integers in the interval [a, b]
  def cube(x: Int): Int = x * x * x               //> cube: (x: Int)Int
  def sumCubes(a: Int, b: Int): Int = {
    if (a > b) 0 else cube(a) + sumCubes(a + 1, b)
  }                                               //> sumCubes: (a: Int, b: Int)Int
  
  // Sum of all factorial of all integers in the interval [a, b]
  def fact(n: Int): Int = if (n == 0) 1 else n * fact(n - 1)
                                                  //> fact: (n: Int)Int
  def sumFacts(a: Int, b: Int): Int = {
    if (a > b) 0 else fact(a) + sumFacts(a + 1, b)
  }                                               //> sumFacts: (a: Int, b: Int)Int
  
  // all these are similar and are derived mathematically from sigma(a,b) f(n)
  
  // this can be represented in programming as well
  def sum(f: Int => Int, a: Int, b: Int): Int = {
    if(a > b) 0 else f(a) + sum(f, a + 1, b)
  }                                               //> sum: (f: Int => Int, a: Int, b: Int)Int
  
  // so now we can redefine the above functions as
  def sumInts2(a: Int, b: Int): Int = sum(id, a, b)
                                                  //> sumInts2: (a: Int, b: Int)Int
  def sumCubes2(a: Int, b: Int): Int = sum(cube, a, b)
                                                  //> sumCubes2: (a: Int, b: Int)Int
  def sumFacts2(a: Int, b: Int): Int = sum(fact, a, b)
                                                  //> sumFacts2: (a: Int, b: Int)Int
  
  // but giving a name for even simple functions which are passed
  // into a higher order function, which is used only once...
  // so we use anonymous functions
  def sumInts3(a: Int, b: Int): Int = sum(x => x, a, b)
                                                  //> sumInts3: (a: Int, b: Int)Int
  def sumCubes3(a: Int, b: Int): Int = sum(x => x * x * x, a, b)
                                                  //> sumCubes3: (a: Int, b: Int)Int
  def sumFacts3(a: Int, b: Int): Int = sum(x => if (x == 0) 1 else x * fact(x - 1), a, b)
                                                  //> sumFacts3: (a: Int, b: Int)Int
  
  // Exercise: tail-recursive version of sum() function defined above
  def sum2(f: Int => Int, a: Int, b: Int): Int = {
    def iter(acc: Int, a: Int): Int = {
      if(a > b) acc
      else iter(acc + f(a), a + 1)
    }
    iter(0, a)
  }                                               //> sum2: (f: Int => Int, a: Int, b: Int)Int
  // testing our exercise implementation
  sum2(x => x, 3, 5)                              //> res0: Int = 12
   
}
