/* NOTE: My comments will be in block comments,
 * the interpreter will comment with inline comments
 */

object mod01_basics {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  /* Expressions, any mathematical expression */
  1 + 1                                           //> res0: Int(2) = 2

  /* The result of an expression can be stored as well (called value)... */
  val two = 1 + 1                                 //> two  : Int = 2

  /* But you cannot change the binding to a value (immutable?)
   * uncomment to see error: reassignment to a val
   */
  // two = 3

  /* if you desperately want to change the binding, then use var instead */
  var name = "Thirumal"                           //> name  : String = Thirumal
  name = "Venkat"

  /* Functions */

  /* Here's an example */
  def addOne(x: Int): Int = x + 1                 //> addOne: (x: Int)Int

  val three = addOne(2)                           //> three  : Int = 3

  /* unless you have a recursion you can leave the return type for the compiler to
   * decide it for you
   */
  def newThree() = 1 + 2                          //> newThree: ()Int

  /* Calling the above function */
  newThree()                                      //> res1: Int = 3

  /* Calling the above function, without parens but very confusing */
  newThree                                        //> res2: Int = 3

  /* Anonymous functions */
  (x: Int) => x + 1                               //> res3: Int => Int = <function1>

  /* Calling anonymous functions are a hassle in a worksheet */
  val addOneUp = (x: Int) => x + 1                //> addOneUp  : Int => Int = <function1>

  /* Call the defined function */
  addOneUp(20)                                    //> res4: Int = 21

  /* Functions can be made up of multiple expressions { } gives us some breathing room */
  def timesTwoPlusLog(x: Int): Int = {
    println("Hello, World!")
    x * 2
  }                                               //> timesTwoPlusLog: (x: Int)Int

  /* This should first print out Hello, World and then 10 in the next line */
  timesTwoPlusLog(5)                              //> Hello, World!
                                                  //| res5: Int = 10

  /* TODO: Explore whether assigning anonymous function definitions
		 * to values will result in loading the function into memory
		 * only during execution like that in Javascript?
		 *
		 * The tradeoff between keeping the function in memory vs. loading it everytime
		 */

  /* Partial application of a function */

  /* Inuition: Used to create specialized functions out of generalized ones */

  /* Let's say we have a function addTwoNums, adds two numbers */
  def addNums(m: Int, n: Int): Int = m + n        //> addNums: (m: Int, n: Int)Int

  addNums(5, 6)                                   //> res6: Int = 11

  /* We want to create a specialized function to add 10 to a number */
  val addTenTo = addNums(10, _: Int)              //> addTenTo  : Int => Int = <function1>

  addTenTo(50)                                    //> res7: Int = 60

  /* The parameter can be any of those, not just the last one */
  val addTwentyTo = addNums(_: Int, 20)           //> addTwentyTo  : Int => Int = <function1>

  addTwentyTo(50)                                 //> res8: Int = 70

  /* Curried functions */

  def multiply(m: Int)(n: Int): Int = m * n       //> multiply: (m: Int)(n: Int)Int

  /* Call this function directly... */
  multiply(2)(3)                                  //> res9: Int = 6

  val timesTwo = multiply(2) _                    //> timesTwo  : Int => Int = <function1>

  timesTwo(3)                                     //> res10: Int = 6

  /* TODO: http://stackoverflow.com/questions/218025/what-is-the-difference-between-currying-and-partial-application */

  /* let's curry our addNums function */
  val curriedAdd = (addNums _).curried            //> curriedAdd  : Int => (Int => Int) = <function1>

  val addThirty = curriedAdd(30)                  //> addThirty  : Int => Int = <function1>

  addThirty(100)                                  //> res11: Int = 130

  /* Variable length arguments for a function */
  /* TODO: Not so clear with map function's usage here... */
  def capitalizeAll(args: String*) = {
    args.map {
      arg => arg.capitalize
    }
  }                                               //> capitalizeAll: (args: String*)Seq[String]

  capitalizeAll("thirumal", "venkat")             //> res12: Seq[String] = ArrayBuffer(Thirumal, Venkat)

  /* Scala classes */
  class Calculator(brand: String) {
    /* Constructor */
    var color: String = if (brand == "HP") {
      "black"
    } else if (brand == "IBM") {
      "blue"
    } else {
      "red"
    }

    /* Instance methods */
    def add(m: Int, n: Int): Int = m + n
  }

  val calc = new Calculator("IBM")                //> calc  : mod01_basics.Calculator = mod01_basics$$anonfun$main$1$Calculator$1
                                                  //| @3a03464

  calc.add(1, 2)                                  //> res13: Int = 3
  
  calc.color                                      //> res14: String = blue
  
  // calc.brand /* Note that, brand is not an instance variable */
  
}
