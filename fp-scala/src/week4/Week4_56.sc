package week4

object Week4_56 {
  println("Decomposition")
  // Suppose you want to write a small interpreter for arithmetic
  // Expr1essions. To keep it simple we'll just concern ourselves
  // only with Number1s and additions.

  // Approach #1:
  // An example hierarchy would be to have a trait 'Expr1'
  // and two subclasses 'Number1' and 'Sum1' with definitions like below
  trait Expr1 {
    def isNumber1: Boolean // classification method (or test method)
    def isSum1: Boolean    // classification method
    def numValue: Int     // accessor method
    def leftOp: Expr1      // accessor method
    def rightOp: Expr1     // accessor method
  }
  // Number1 class
  class Number1(val numValue: Int) extends Expr1 {
    def isNumber1: Boolean = true
    def isExpr1: Boolean = false
    def leftOp: Expr1 = throw new Error("Number1.leftOp")
    def rightOp: Expr1 = throw new Error("Number1.rightOp")
  }
  // Sum1 class
  class Sum1(val leftOp: Expr1, val rightOp: Expr1) extends Expr1 {
    def isNumber1: Boolean = false
    def isExpr1: Boolean = true
    def numValue: Int = throw new Error("Sum1.numValue")
  }
  // Evaluate the Expr1ession
  def eval1(e: Expr1): Int = {
    if (e.isNumber1) e.numValue
    else if (e.isSum1) eval1(e.leftOp) + eval1(e.rightOp)
    else throw new Error("Unknown Expr1ession: " + e)
  }
  // Writing these classification methods and accessor functions
  // quickly become tedious!!
  // Suppose you want to extend Expr1 to handle products and variable names
  // we're talking 25 new methods. This grows quadratically with the
  // Number1 of subclasses. This is bad news.

  // Approach #2:
  // We can use isInstanceOf and asInstanceOf, this avoids the use of
  // classification methods and we'd need only the accessor methods for
  // classes where values were defined. But this is very low-level and
  // potentially unsafe. This is the reason I'm not even going to give you
  // folks examples of the above.

  // Approach #3:
  // Another implementation would be something like below
  // But this would only suit if you only wanted to evaluate the expression
  // if you wanted to add a new method you'll have to touch each and every
  // class in the hierachy and add a method to do the same.
  // This again becomes very messy.
  trait Expr2 {
    def eval: Int
  }
  class Number2(n: Int) extends Expr2 {
    def eval: Int = n
  }
  class Sum2(e1: Expr2, e2: Expr2) extends Expr2 {
    def eval: Int = e1.eval + e2.eval
  }
  // There's even more fundamental flaw with this object oriented decomposition
  // scheme. Suppose, if you wanted to simplify the expressions, say using the
  // rule:
  // a * b + a * c -> a * (b + c)
  // This is a non-local simplification, It cannot be encapsulated in the
  // method of a single object!

  // The task we're setting out to solve: Find a general and
  // a convinient way to access objects in an extensible
  // class hierarchy.

  // The main idea behind classification and accessor functions is to see
  // 1) which subclass is being used
  // 2) identify the arguments of the constructor.

  // Such a situation is very common in functional languages, it is automated.
  // This is called as "Pattern Matching"

  // Before we use and know about pattern matching, we need to know about =
  // Case classes, are very similar to classes. It is preceeded by the word case
  trait Expr {
    def eval: Int = this match {
	    case Number(n) => n
	    case Sum(e1, e2) => e1.eval + e2.eval
	    case Prod(e1, e2) => e1.eval * e2.eval
	    case Var(_) => throw new Error("Var.eval")
	  }
	  private def printExpr(e1: String, e2: String, op: String) = {
	   "(" + e1 + " " + op +  "  " + e2 + ")"
	  }
	  def show: String = this match {
      case Var(x) => x
	    case Number(n) => n.toString
	    case Sum(e1, e2)  => printExpr(e1.show, e2.show, "+")
	    case Prod(e1, e2) => printExpr(e1.show, e2.show, "*")
	  }
  }
  case class Number(n: Int) extends Expr
  case class Sum(e1: Expr, e2: Expr) extends Expr
  case class Prod(e1: Expr, e2: Expr) extends Expr
  case class Var(x: String) extends Expr

  // What do you get when we add this 'case' keyword?
  // The Scala compiler implicitly adds a companion object
  // so you automatically get:
  /*
  object Number {
    def apply(n: Int) = new Number(n)
  }
  object Sum {
    def apply(e1: Expr, e2: Expr) = new Sum(e1, e2)
  }
  */
  // You can just write 'Number(1)' instead of 'new Number(1)'

  // So how do we express our eval() function?
  // Using pattern matching as shown inside the trait

  // The above is matched in the order you have written them.
  // A MatchError exception is thrown if none of the patterns match.

  // What kinds of patterns can you match?
  // 1) Constructors
  //      Number(n) : Matches Number(n) and gives us the variable n to use
  //      Number(_) : matches the Number(<some number>) but does not care about
  //                  the argument provided
  // 2) Constants like 1, true, "abc" or some constant val (val PI = 3.14159)
  //    The convention is to use capital letters for constants in Scala
  //    The only exception for constant names are null, true, false,...
  //
  // NOTE: A variable can only occur once in a pattern. So, Sum(x, x) is not
  // a valid pattern.

  // To choose Approach #3 or Pattern matching?
  // If you're going to be declaring more subclasses in the future the OO approach
  // has the upper hand. But if you keep adding functionality to these methods
  // then you have an upper hand if you use pattern matching.
  //
  // This tradeoff is a huge problem faced by a lot of people.
  // It has its own name: "The Expression Problem"
}