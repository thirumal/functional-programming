object Week3 {
	/**
	 * Abstract classes need not be completely defined
	 * and also you cannot instantiate abstract classes
	 */
	abstract class IntSet {
	  /** Include x in the existing set and return a new one */
	  def incl(x: Int): IntSet
	  /** Take a union of this and that and return a new set */
	  def union(that: IntSet): IntSet
	  /** Does the set contain this element x? */
	  def contains(x: Int): Boolean
	}
	
	// sets are implemented as a binary search tree
	// subtrees are subsets and therefore
	// instead of null we'll have empty set as leaves
	
	/*
	 * NOTE: Object class cannot have a value parameter
	 * so don't dream about object Empty(...) { ... }
	 */
	/** Object class definition for an empty set */
	object Empty extends IntSet {
	  /** Contains function,  always empty, no element is included */
	  def contains(x: Int): Boolean = false
	  /** Include x and return a new set */
	  def incl(x: Int) = new NonEmpty(x, Empty,   Empty)
	  /** Union */
	  def union(that: Int) = that
	  /** Convert empty set to a string */
	  override def toString() = "."
	}
	
	/** Class definition for a non empty set */
	class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
	  /** Contains function, uses sortedness of the tree */
	  def contains(x: Int): Boolean = {
	    if (x < elem) left contains x
	    else if (x > elem) right contains x
	    else true
	  }
	  /** Include x and return a new set */
	  def incl(x: Int): IntSet = {
	    if (x < elem) new NonEmpty(elem, left incl x, right)
	    else if (x > elem) new NonEmpty(elem, left, right incl x)
	    else this
	  }
	  /** Union this with that */
	  def union(that: IntSet): IntSet = {
	    ((left union right) union that) incl elem
	  }
	  /** Convert a none empty set to a string */
	  override def toString() = "{" + left + elem + right + "}"
	}
	
	/* When to override? */
	abstract class BaseClass {
	  /** Variable already defined */
	  def foo = 1
	  /** Abstract variable */
	  def bar: Int
	}
	
	class SubClass extends BaseClass {
	  /** Override the variable already defined in base class */
	  override def foo = 2
	  /** Define the abstract variable here so that we can be instantiated */
	  def bar = 3
	}
	
	
	/*
	Dynamic binding, code invoked by a method call depends on the runtime type
	of the object that contains the method
	
	A class can have only one superclass, this is too constraining...
	
	this is where traits come into picture.. this is just like an abstract class
	from which many classes can inherit code
	
	A class can inherit from many traits
	
	traits resemble Java interfaces, but traits are more powerful than interfaces
	as traits can themselves contain concrete method implementations wheras
	interfaces could only contain abstract methods
	
	what traits cannot have are value parameters
	the below line is invalid
	trait Movable(...) { ... }
	
	An example of a valid trait:
	*/
	
	trait Planar {
	  def height: Int
	  def width: Int
	  def surface = height * width
	}
	
	/*

	Assume you're declaring a class Square extending Shape with traits Planar and Movable
	then the syntax would be
	
	class Square extends Shape with Planar with Movable ... {
	  ...
	}
  
	*/
	
	/* Scala type hierarchy and exceptions, see slides */
	
	/* Null and Nothing, see slides */

  val t1 = new NonEmpty(3, Empty, Empty)
  val t2 = t1 incl 4
}