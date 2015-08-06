package week2

object Week2_3 {
  // fixed point of a function

  // a number x is called a fixed point of a function f if
  // f(x) = x

  // we can locate a function's fixed point by starting with some guess
  // repetitively applying f
  // x, f(x), f(f(x)), ...
  // until we don't vary too much

  val DELTA = 0.00001                             //> DELTA  : Double = 1.0E-5
  val INITIAL_GUESS = 1.0                         //> INITIAL_GUESS  : Double = 1.0

  // Helper function
  def abs(x: Double): Double = if (x < 0) -x else x
                                                  //> abs: (x: Double)Double

  def fixedPoint(f: Double => Double)(firstGuess: Double): Double = {
    // function to test if we need to iterate further?
    def isCloseEnough(x: Double, y: Double): Boolean = {
      abs(y / x - 1) < DELTA
    }
    // iterate function
    def iter(guess: Double): Double = {
      val nextGuess = f(guess)
      if (isCloseEnough(guess, nextGuess)) guess
      else iter(nextGuess)
    }
    // finally call the recursive function we have
    iter(firstGuess)
  }                                               //> fixedPoint: (f: Double => Double)(firstGuess: Double)Double

  // we know that fixed point of f(x) = 1 + x / 2 is 2
  fixedPoint(x => 1 + x / 2)(INITIAL_GUESS)       //> res0: Double = 1.999969482421875

  // Defining square root as fixed point
  // y = f(x) = sqrt(x) such that y * y = x
  // or in other words
  // sqrt(x) = number y such that y = x / y
  // but this does not converge
  // last week we used the following function
  // ( g + x / g ) / 2
  // also known as the average damping function
  // therefore we can define square root as following
  def sqrt(x: Double): Double = {
    // damping function
    def dampingFunction(g: Double): Double = (g + x / g) / 2
    // use the damping function with the fixed point to get square root
    fixedPoint(dampingFunction)(INITIAL_GUESS)
  }                                               //> sqrt: (x: Double)Double
  // testing square root
  sqrt(2)                                         //> res1: Double = 1.4142156862745097

  // you can redefine average damping function as follows
  def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2
                                                  //> averageDamp: (f: Double => Double)(x: Double)Double

  // Now square root can be defined as
  def sqrt2(x: Double): Double = fixedPoint(averageDamp(y => x / y))(INITIAL_GUESS)
                                                  //> sqrt2: (x: Double)Double
  // test the new implementation
  sqrt(2)                                         //> res2: Double = 1.4142156862745097
}
