object Exercise0 {
  /**
   * This method computes the sum of all elements in the list xs. There are
   * multiple techniques that can be used for implementing this method, and
   * you will learn during the class.
   *
   * For this example assignment you can use the following methods in class
   * `List`:
   *
   *  - `xs.isEmpty: Boolean` returns `true` if the list `xs` is empty
   *  - `xs.head: Int` returns the head element of the list `xs`. If the list
   *    is empty an exception is thrown
   *  - `xs.tail: List[Int]` returns the tail of the list `xs`, i.e. the the
   *    list `xs` without its `head` element
   *
   *  ''Hint:'' instead of writing a `for` or `while` loop, think of a recursive
   *  solution.
   *
   * @param xs A list of natural numbers
   * @return The sum of all elements in `xs`
   */
  def sum(xs: List[Int]): Int = {
    def tailSum(acc: Int, xs: List[Int]): Int = {
      if (xs.isEmpty) acc
      else tailSum(acc + xs.head, xs.tail)
    }
    tailSum(0, xs)
  }                                               //> sum: (xs: List[Int])Int

  // Tests to run for sum on a list
  sum(List()) == 0                                //> res0: Boolean = true
  sum(List(-1)) == -1                             //> res1: Boolean = true
  sum(List(1, 2)) == 3                            //> res2: Boolean = true
  sum(List(-1, -2, -3)) == -6                     //> res3: Boolean = true

  /**
   * This method returns the largest element in a list of integers. If the
   * list `xs` is empty it throws a `java.util.NoSuchElementException`.
   *
   * You can use the same methods of the class `List` as mentioned above.
   *
   * ''Hint:'' Again, think of a recursive solution instead of using looping
   * constructs. You might need to define an auxiliary method.
   *
   * @param xs A list of natural numbers
   * @return The largest element in `xs`
   * @throws java.util.NoSuchElementException if `xs` is an empty list
   */
  def max(xs: List[Int]): Int = {
    def tailMax(max: Int, xs: List[Int]): Int = {
      if(xs.isEmpty) max
      else {
        val newMax = if(xs.head > max) xs.head else max
        tailMax(newMax, xs.tail)
      }
    }
    // referencing xs.head on an empty list
    // will automatically cause a NoSuchElementException
    tailMax(xs.head, xs.tail)
  }                                               //> max: (xs: List[Int])Int

  max(List(1)) == 1                               //> res4: Boolean = true
  max(List(5,9,7)) == 9                           //> res5: Boolean = true
  max(List(1, -2, 0)) == 1                        //> res6: Boolean = true
  max(List(-3, -2, -1)) == -1                     //> res7: Boolean = true
  // finally testing the error case on an empty list
  try {
    max(List())
  } catch {
    case e: Exception => println("Exception thrown: " + e)
  }                                               //> Exception thrown: java.util.NoSuchElementException: head of empty list
                                                  //| res8: AnyVal = ()
}