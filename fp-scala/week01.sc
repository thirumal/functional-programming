package week01

import scala.annotation.tailrec

object lecture {
  // Customary Hello world example
  println("Hello, World")                         //> Hello, World

  // Arithmetic operations on integers
  val x = 15                                      //> x  : Int = 15
  val y = 10                                      //> y  : Int = 10
  x + y                                           //> res0: Int = 25
  x - y                                           //> res1: Int = 5
  x * y                                           //> res2: Int = 150
  x / y                                           //> res3: Int = 1
  x % y                                           //> res4: Int = 5

  // Comparision operations
  x < y                                           //> res5: Boolean = false
  x <= y                                          //> res6: Boolean = false
  x > y                                           //> res7: Boolean = true
  x >= y                                          //> res8: Boolean = true
  x == y                                          //> res9: Boolean = false
  x == x                                          //> res10: Boolean = true
  x != y                                          //> res11: Boolean = true
  x != x                                          //> res12: Boolean = false

  // Boolean operations
  val bool1 = true                                //> bool1  : Boolean = true
  val bool2 = false                               //> bool2  : Boolean = false
  bool1 && bool1                                  //> res13: Boolean = true
  bool2 && bool2                                  //> res14: Boolean = false
  bool1 && bool2                                  //> res15: Boolean = false
  bool1 || bool1                                  //> res16: Boolean = true
  bool2 || bool2                                  //> res17: Boolean = false
  bool1 || bool2                                  //> res18: Boolean = true
  !bool1                                          //> res19: Boolean = false
  !bool2                                          //> res20: Boolean = true

  // Conditional
  if (true) x else y                              //> res21: Int = 15
  if (false) x else y                             //> res22: Int = 10

  // Function definition
  // def <name> (params...) : <Return type> =
  //   ...
  //   ...
  def add(a: Int, b: Int): Int = a + b            //> add: (a: Int, b: Int)Int
  // Great we've defined them, let's call it
  add(x, y)                                       //> res23: Int = 25

  // Difference between def and val
  // Let's define an infinite loop (a recursive function)
  // Recursive functions must specify a return type
  def loop: Void = loop                           //> loop: => Void
  // val evaluates the right hand side
  def myloop = loop                               //> myloop: => Void
  // def just defines the right hand side
  // val myloop2 = loop
  // NOTE: The above statement does not
  // terminate if uncommented

  /* Method to compute the absolute value of a number */
  def abs(x: Double) = if (x < 0) -x else x       //> abs: (x: Double)Double
  abs(5.7)                                        //> res24: Double = 5.7
  abs(-50.678)                                    //> res25: Double = 50.678
  abs(0)                                          //> res26: Double = 0.0

  /* Method to compute square root by Netwon's method */
  def sqrt(x: Double) = {
    // This returns true if the guess error level is acceptable
    def isGoodEnough(g: Double) = abs(g * g - x) / x < 0.0001
    
    // This improves the guess for a better square root
    def improve(g: Double) = (g + x / g) / 2
      
    // The iterative square root method
    def sqrtIter(g: Double): Double = if(isGoodEnough(g)) g else sqrtIter(improve(g))
    
    sqrtIter(1.0)
  }                                         //> sqrt: (x: Double)Double
  // Test your sqrt() routine
  sqrt(2)                                   //> res27: Double = 1.4142156862745097
  sqrt(1e-30)                               //> res28: Double = 1.0000000300245242E-15
  sqrt(1e+60)                               //> res29: Double = 1.0000000031080746E30

  // GCD routine
  def gcd(a: Int, b: Int): Int = {
    if(b == 0) a else gcd(b, a % b)
  }                                         //> gcd: (a: Int, b: Int)Int
  // Test out this routine
  gcd(4, 3)                                 //> res30: Int = 1
  gcd(14, 21)                               //> res31: Int = 7
  gcd(8, 8)                                 //> res32: Int = 8
  gcd(27, 4)                                //> res33: Int = 1
  
  // Factorial
  def factorial(n: Int): Int = if(n == 0) 1 else n * factorial(n - 1)
                                                  //> factorial: (n: Int)Int
  // Tester routines for factorial
  factorial(1)                              //> res34: Int = 1
  factorial(0)                              //> res35: Int = 1
  factorial(4)                              //> res36: Int = 24
  factorial(6)                              //> res37: Int = 720
  
  // Tail recursive factorial definition
  def fact(n: Int) = {
    @tailrec
    def factIter(acc: Int, n: Int): Int = {
      if(n == 0) acc
      else factIter(n * acc, n - 1)
    }
    factIter(1, n)
  }                                         //> fact: (n: Int)Int

  // Test for tail recursive factorial
  fact(0)                                   //> res38: Int = 1
  fact(1)                                   //> res39: Int = 1
  fact(4)                                   //> res40: Int = 24
  fact(6)                                   //> res41: Int = 720
}