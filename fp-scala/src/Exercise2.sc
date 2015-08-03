/**
 * 2. Purely Functional Sets.
 */
import scala.annotation.tailrec

object Exercise2 {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)
                                                  //> contains: (s: Exercise2.Set, elem: Int)Boolean

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Set = (x: Int) => x == elem
                                                  //> singletonSet: (elem: Int)Exercise2.Set

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set = (x: Int) => contains(s, x) || contains(t, x)
                                                  //> union: (s: Exercise2.Set, t: Exercise2.Set)Exercise2.Set

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: Set, t: Set): Set = (x: Int) => contains(s, x) && contains(t, x)
                                                  //> intersect: (s: Exercise2.Set, t: Exercise2.Set)Exercise2.Set

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
  def diff(s: Set, t: Set): Set = (x: Int) => contains(s, x) && !contains(t, x)
                                                  //> diff: (s: Exercise2.Set, t: Exercise2.Set)Exercise2.Set

  /**
   * Returns the subset of `s` for which `p` holds.
   */
  def filter(s: Set, p: Int => Boolean): Set = intersect(s, p)
                                                  //> filter: (s: Exercise2.Set, p: Int => Boolean)Exercise2.Set

  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000                                //> bound  : Int = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
  def forall(s: Set, p: Int => Boolean): Boolean = {
    @tailrec
    def iterate(a: Int): Boolean = {
      if (a > bound) true // end of bound, every element in s was in p, true
      else if (diff(s, p)(a)) false // in s, not in p.. return false
      else iterate(a + 1) // neither, go to next element
    }
    iterate(-bound)
  }                                               //> forall: (s: Exercise2.Set, p: Int => Boolean)Boolean

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
  def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, x => !p(x))
                                                  //> exists: (s: Exercise2.Set, p: Int => Boolean)Boolean

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
  def map(s: Set, f: Int => Int): Set = (x: Int) => exists(s, (y: Int) => f(y) == x)
                                                  //> map: (s: Exercise2.Set, f: Int => Int)Exercise2.Set

  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }                                               //> toString: (s: Exercise2.Set)String

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }                                               //> printSet: (s: Exercise2.Set)Unit
}