package week2

// i'm expecting folks to know what objects and classes are...
// a better place to learn them would be
// https://docs.oracle.com/javase/tutorial/java/concepts/index.html

object Week2_567 {
  // objects and classes
  
  // both values and types are in different namespaces
  // so the following line is cool, and does not matter to the compiler
  // but better avoid such things as they're confusing to people
  val Rational = "Awesome, no conflict!"          //> Rational  : String = Awesome, no conflict!

  // new element of type Rational can be created as follows
  val x = new Rational(1, 3)                      //> x  : Rational = 1 / 3
  val y = new Rational(5, 7)                      //> y  : Rational = 5 / 7
  val z = new Rational(3, 2)                      //> z  : Rational = 3 / 2
  
  x.numer                                         //> res0: Int = 1
  x.denom                                         //> res1: Int = 3
  
  x - y - z                                       //> res2: Rational = -79 / 42
  y + y                                           //> res3: Rational = 10 / 7
  
  x < y                                           //> res4: Boolean = true
  x max y                                         //> res5: Rational = 5 / 7
  
  // To guard against faulty arguments
  // or a precondiction failure
  // we can use "require"
  // for ex. folks can abuse our class like this
  // val strange = new Rational(1, 0)
  // strage.add(strange)
  
  // there's also assert, but to check stuff inside functions
  assert(1 == 1)
  
  val a = new Rational(2)                         //> a  : Rational = 2 / 1
  
  // infix notation x.sub(y) can be written as x sub y
  // x.sub(y)
  // x sub y
  
  x - y / z                                       //> res6: Rational = 1 / -7
}

// class to define:
// rational numbers p / q, p is a numerator and q is the denominator

// by defining a class we are defining
// * a new `type` called Rational
// * a `constructor` Rational to create elements of this type

/**
 * Rational Number class
 */
class Rational(x: Int, y: Int) {

  require(y != 0, "denominator should not be zero")

  /** GCD of x and y (private) */
  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
  
  // find the gcd of x and y
  private val g = gcd(x, y)
  
  /** Numerator */
  val numer = x / g
  /** Denominator */
  val denom = y / g
  
  /** Another constructor */
  def this(x: Int) = this(x, 1)
  
  /** Method to add two rational numbers */
  def  + (that: Rational): Rational = {
    new Rational(numer * that.denom + denom * that.numer, denom * that.denom)
  }
  
  /** Method to negate a rational number */
  def unary_- : Rational = new Rational(-numer, denom)
  
  /** Method to subtract two rational numbers */
  def - (that: Rational): Rational =  this + (-that)
  
  /** Method to multiply two rational numbers */
  def * (that: Rational): Rational = {
    new Rational(numer * that.numer, denom * that.denom)
  }
  
  /** Method to invert a rational number */
  def invert: Rational = {
    new Rational(denom, numer)
  }
  
  /** Method to divide two rational numbers */
  def / (that: Rational): Rational = this * that.invert
  
  /** Less than comparision operation */
  def < (that: Rational): Boolean = numer * that.denom < denom * that.numer
  
  /** Maximum function */
  def max(that: Rational): Rational = {
    if (this < that) that
    else this
  }
  
  /** Method to convert rational numbers into a string */
  override def toString(): String = {
    numer + " / " + denom
   }
}
