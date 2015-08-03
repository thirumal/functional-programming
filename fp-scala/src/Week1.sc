import scala.annotation.tailrec

object Week1 {
  // Customary hello world
  println("Hello, Scala!")                        //> Hello, Scala!
  
  // you can use scala as a calculator
  34 + 46                                         //> res0: Int(80) = 80
  
  // this is how you define a variable
  def radius = 10                                 //> radius: => Int
  
  // another way of defining a variable
  val pi = 3.14159                                //> pi  : Double = 3.14159
  
  // there's another way var, for some reason Odersky has not spoken about this
  // in his lectures for now
  
  // this is an expression
  2 * pi * radius                                 //> res1: Double = 62.8318
  
  // define a function
  def square(x: Double) = x * x                   //> square: (x: Double)Double
  
  // call a function / evaluate it
  square(2)                                       //> res2: Double = 4.0
  
  // below is equivalent to square(9)
  square(5 + 4)                                   //> res3: Double = 81.0
  
  // f o f (x)
  square(square(4))                               //> res4: Double = 256.0
  
  // compose functions from functions
  def sumOfSquares(x: Double, y: Double) = {
    square(x) + square(y)
  }                                               //> sumOfSquares: (x: Double, y: Double)Double
  
  sumOfSquares(3, 4)                              //> res5: Double = 25.0
  
  // This definition is non-terminating
  // loop evalutes to loop, further it evaluates again to loop and so on...
  def loop: Int = loop                            //> loop: => Int
  
  // call by value: all function arguments are evaluated when a function is called
  // call by name: function evaluations are evaluated only when an expression involves that arg.
  
  // by default scala uses call by value for expression evaluation
  
  // advantages:
  // call by value = every function argument is evaluated only once
  // call by name = function argument is not evaluated if not used
  
  // Theorem: If CBV terminates => CBN terminates as well, but vice versa not true
  // Proof:
  def first(x: Int, y: Int) = x                   //> first: (x: Int, y: Int)Int
  // first(1, loop) <= does not terminate
  
  // you can force an argument to be evaulated in call by name,
  // by using => before the type is specified in the argument (as shown below)
  def first2(x: Int, y: => Int) = x               //> first2: (x: Int, y: => Int)Int
  // y is evaluated as call by name
  first2(1, loop)                                 //> res6: Int = 1
  
  // choose between two alternatives
  // if else is used for expressions not statements
  def abs(x: Double) = if(x < 0) -x else x        //> abs: (x: Double)Double
  
  // boolean constants
  true                                            //> res7: Boolean(true) = true
  false                                           //> res8: Boolean(false) = false
  
  val a = true                                    //> a  : Boolean = true
  val b = false                                   //> b  : Boolean = false
  // negation
  !a                                              //> res9: Boolean = false
  // conjunction
  a && a                                          //> res10: Boolean = true
  // disjunction
  b || b                                          //> res11: Boolean = false
  // short-circuit evaluation
  true && a                                       //> res12: Boolean = true
  false && a                                      //> res13: Boolean = false
  true || b                                       //> res14: Boolean = true
  false || b                                      //> res15: Boolean = false
  
  // Java comparision operations are also there
  // thumb rule: If an expression evaluates in Java, then  you can
  // expect it to do so in Scala as well
  
  // for function arguments evaluation we have call by name and call by value
  // similary we have define by name and define by value to define them as well
  // ex. for define by value
  val x = 2                                       //> x  : Int = 2
  // ex. for define by name
  // if we define by name, an expression is only evaluated when used
  // for example
  def y = square(x)                               //> y: => Double
  y                                               //> res16: Double = 4.0
  
  // define AND and OR functions without using && and ||
  def and(x: Boolean, y: => Boolean) = if (x) y else false
                                                  //> and: (x: Boolean, y: => Boolean)Boolean
  def or(x: Boolean, y: => Boolean) = if(x) true else y
                                                  //> or: (x: Boolean, y: => Boolean)Boolean
  and(false, false)                               //> res17: Boolean = false
  and(false, true)                                //> res18: Boolean = false
  and(true, false)                                //> res19: Boolean = false
  and(true, true)                                 //> res20: Boolean = true
  or(false, false)                                //> res21: Boolean = false
  or(false, true)                                 //> res22: Boolean = true
  or(true, false)                                 //> res23: Boolean = true
  or(true, true)                                  //> res24: Boolean = true
  
  // notice that second parameter to or and and are made call by name
  // this helps us evaluate this function in short-circuit mode
  // and terminate in non-terminating conditions (if possible)
  def loop2: Boolean = loop2                      //> loop2: => Boolean
  or(true, loop2)                                 //> res25: Boolean = true
  and(false, loop2)                               //> res26: Boolean = false
  
  // block
  // define a value outside the block
  val m = 0                                       //> m  : Int = 0
  // shadow the definition of m inside the block
  val res = {
    val m = 3
    m * m
  }                                               //> res  : Int = 9
  // definition of m has been unchanged outside the block
  m                                               //> res27: Int = 0

  // very long expressions, can be expressed as below
  val longexpr1 = (x
          + x)                                    //> longexpr1  : Int = 4
  val longexpr2 = x +
           x                                      //> longexpr2  : Int = 4
  
  // complete square root example
  // external dependency (square function, defined above)
  def sqrt(x: Double): Double = {
    // delta for comparision, don't go too low (lesser than double can represent)
    val DELTA = 0.00000000000001
    // Initial guess to use while starting the estimation
    val INITIAL_GUESS = 1.0
    // Function to check if guess is good enough?
    def isGoodEnough(g: Double): Boolean = {
      abs(square(g) / x - 1) < DELTA
    }
    // Function to improve the guess
    def improveGuess(g: Double): Double = {
      (g + x / g) / 2
    }
    // Recursive square root impelementation
    def sqrtRecr(g: Double): Double = {
      if(isGoodEnough(g)) g
      else sqrtRecr(improveGuess(g))
    }
    // Call the recursive implementation
    if(abs(x) < DELTA) { // x ~ 0
      0.0
    } else if(-x > DELTA) { // x < 0
      throw new IllegalArgumentException(x + "< 0 ?")
    } else { // x > 0
      sqrtRecr(1.0)
    }
  }                                               //> sqrt: (x: Double)Double
  
  // tests for sqrt() routine
  sqrt(0.0)                                       //> res28: Double = 0.0
  sqrt(1e-6)                                      //> res29: Double = 0.001
  sqrt(1.0)                                       //> res30: Double = 1.0
  sqrt(2.0)                                       //> res31: Double = 1.414213562373095
  sqrt(64.0)                                      //> res32: Double = 8.0
  sqrt(100.0)                                     //> res33: Double = 10.0
  
  // tail recursion
  
  // if a function calls itself as its last action, the function's stack frame
  // can be reused. Hence your recursion depth does not increase
  // this concept of writing a function in this fashion is called tail recursion
  // the call made to itself inside a tail recursive function is called a tail-call
  
  // annonate tail recursive functions using the @tailrec
  // don't forget to import this guy: import scala.annotation.tailrec
  // for some reason eclipse worksheet does not terminate if I add this
  // annotation
  
  // a good beginner's example of a tail recursive function is
  // Euclid's algorithm for GCD
  //@tailrec
  def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
                                                  //> gcd: (a: Int, b: Int)Int
  // try it out
  gcd(14, 21)                                     //> res34: Int = 7
  
  // Factorial
  //
  // this is not tail recursive because
  // the last expression is pending evaluation when fact is called again
  // for ex: fact(4) = 4 * (3 * fact(2)) 4 * 3 *... is still not evaluated
  def fact(n: Int): Int = if(n == 0) 1 else n * fact(n - 1)
                                                  //> fact: (n: Int)Int
  fact(5)                                         //> res35: Int = 120
  
  // tail recursive version of factorial
  def factTR(n: Int): Int = {
    //define an accumulator
    @tailrec
    def iter(acc: Int, r: Int): Int = {
      if(r == 0) acc
      else iter(acc * r, r - 1)
    }
    iter(1, n)
  }                                               //> factTR: (n: Int)Int
  factTR(5)                                       //> res36: Int = 120
}