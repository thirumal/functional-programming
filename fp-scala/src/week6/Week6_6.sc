package week6

object Week6_6 {
  println("Maps")                                 //> Maps
  // Another fundamental collection type in Scala is the 'map'
  // A map of type Map[Key, Value] is a data structure that associates
  // keys of type Key with the values of type Value
  // Examples
  val romanNumerals = Map("I" -> 1, "V" -> 5, "X" -> 10)
                                                  //> romanNumerals  : scala.collection.immutable.Map[String,Int] = Map(I -> 1, V 
                                                  //| -> 5, X -> 10)
  val capitalOfCountry = Map("US" -> "Washington", "Switzerland" -> "Bern")
                                                  //> capitalOfCountry  : scala.collection.immutable.Map[String,String] = Map(US -
                                                  //| > Washington, Switzerland -> Bern)
  // Maps are iterables, but also functions
  capitalOfCountry("US")                          //> res0: String = Washington
  // But calling capitalOfCountry("Andorra") will result in a NoSuchElementException.
  // To prevent this we can use 'get' method on the map instead
  // This returns an object of type 'Option'.
  capitalOfCountry get "Andorra"                  //> res1: Option[String] = None
  capitalOfCountry get "US"                       //> res2: Option[String] = Some(Washington)

  // A possible defintiion of Option would be as follows:
  // trait Option[+A] {
  //   case class Some[+A](value: A) extends Option[A]
  //   object None extends Option[Nothing]
  // }
  // The expression 'map get key' returns:
  // 1) None if map does not contain the given key
  // 2) Some(x) if map associates the given key with the value x

  // Since options are defined as case classes, you can pattern match on them.
  def showCapital(country: String) = capitalOfCountry.get(country) match {
    case Some(capital) => capital
    case None => "Missing Data"
  }                                               //> showCapital: (country: String)String
  // Tests
  showCapital("US")                               //> res3: String = Washington
  showCapital("Andorra")                          //> res4: String = Missing Data

  // Sorted and sortWith
  val fruit = List("apple", "pear", "banana", "pineapple")
                                                  //> fruit  : List[String] = List(apple, pear, banana, pineapple)
  // sorts by length of each word
  fruit.sortWith(_.length < _.length)             //> res5: List[String] = List(pear, apple, banana, pineapple)
  // Sorts lexicographically
  fruit.sorted                                    //> res6: List[String] = List(apple, banana, pear, pineapple)

  // GroupBy
  // Groups all elements starting with the same alphabet
  fruit groupBy(_.head)                           //> res7: scala.collection.immutable.Map[Char,List[String]] = Map(b -> List(ban
                                                  //| ana), p -> List(pear, pineapple), a -> List(apple))

  // More examples
  // A polynomial can be seen as a map from exponents to coefficients
  // For example x^3 - 2x + 5 can be represented with the map as
  Map(0 -> 5, 1 -> -2, 3 -> 1)                    //> res8: scala.collection.immutable.Map[Int,Int] = Map(0 -> 5, 1 -> -2, 3 -> 1
                                                  //| )

  // Let's make some observations
  val a = Map(0 -> 5, 1 -> -2, 3 -> 1)            //> a  : scala.collection.immutable.Map[Int,Int] = Map(0 -> 5, 1 -> -2, 3 -> 1)
                                                  //| 
  val b = Map(0 -> 9, 2 -> -9, 4 -> -1)           //> b  : scala.collection.immutable.Map[Int,Int] = Map(0 -> 9, 2 -> -9, 4 -> -1
                                                  //| )
  // Let's concatenate these two maps, and observe what happens
  a ++ b                                          //> res9: scala.collection.immutable.Map[Int,Int] = Map(0 -> 9, 1 -> -2, 2 -> -
                                                  //| 9, 3 -> 1, 4 -> -1)
  // notice how elements of b superimpose on a
  // if b has elements which are not in a, then they're present
  // in the result. But if b has elements which are already in
  // a then they are replaced by ones present in b

  // Default values
  val c = a withDefaultValue 0                    //> c  : scala.collection.immutable.Map[Int,Int] = Map(0 -> 5, 1 -> -2, 3 -> 1)
                                                  //| 
  // Let's try getting some value whose key is not in map
  c(99)                                           //> res10: Int = 0

  // Based on this observation, let's design a class Polynom that
  // represents polynomials as map
  class Polynom(val terms0: Map[Int, Double]) {
    val terms = terms0 withDefaultValue 0.0
    // Sequence constructor, see p3 & p4
    def this(bindings: (Int, Double)*) = this(bindings.toMap)
    // Addition
    def add (that: Polynom) = new Polynom(terms ++ that.terms.map(adjust))
    private def adjust(term: (Int, Double)): (Int, Double) = {
      val (exp, coeff) = term
      exp -> (coeff + terms(exp))
    }
    // Better implementation of addition
    def + (that: Polynom) = new Polynom((that.terms foldLeft terms)(addTerm))
    private def addTerm(curTerms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
      val (exp, coeff) = term
      curTerms + (exp -> (coeff + curTerms(exp)))
    }
    // Printing the map
    override def toString = {
       (for((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "
    }
  }
  // Tests
  val p1 = new Polynom(Map(1 -> 2.0, 3 -> 4.0, 5 -> 6.2))
                                                  //> p1  : week6.Week6_6.Polynom = 6.2x^5 + 4.0x^3 + 2.0x^1
  val p2 = new Polynom(Map(0 -> 3.0, 3 -> 7.0))   //> p2  : week6.Week6_6.Polynom = 7.0x^3 + 3.0x^0
  p1 add p2                                       //> res11: week6.Week6_6.Polynom = 6.2x^5 + 11.0x^3 + 2.0x^1 + 3.0x^0
  // Instead of passing Map
  // we can pass a sequence of pairs
  // this is handled by the alternate constructor present in the class
  val p3 = new Polynom(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
                                                  //> p3  : week6.Week6_6.Polynom = 6.2x^5 + 4.0x^3 + 2.0x^1
  val p4 = new Polynom(0 -> 3.0, 3 -> 7.0)        //> p4  : week6.Week6_6.Polynom = 7.0x^3 + 3.0x^0
  p3 + p4                                         //> res12: week6.Week6_6.Polynom = 6.2x^5 + 11.0x^3 + 2.0x^1 + 3.0x^0
}