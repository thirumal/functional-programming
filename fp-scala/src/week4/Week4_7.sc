package week4

object Week4_7 {
  println("Lists")                                //> Lists
  // Construct a list of elements x1, ..., xn
  val list1 = List(1, 2, 3, 4, 5)                 //> list1  : List[Int] = List(1, 2, 3, 4, 5)
  // Two important differences between lists and array
  // 1) Lists are immutable, while arrays are mutable
  // 2) Lists are recursive, while arrays are flat

  // All lists are constructed from
  // 1) The empty list Nil
  // 2) The construction operator :: (a.k.a known as Cons)
  // x :: xs gives us a new list with first element as 'x'
  // followed by the elements of list 'xs' (in other words prepend)
  val list2 = 0 :: list1                    //> list2  : List[Int] = List(0, 1, 2, 3, 4, 5)
  // The list of numbers we've constructed above is actually
  val list3 = (0 :: (1 :: (2 :: (3 :: (4 :: (5 :: Nil))))))
                                                  //> list3  : List[Int] = List(0, 1, 2, 3, 4, 5)
  // Customary empty list can also be represented as Nil
  Nil                                       //> res0: scala.collection.immutable.Nil.type = List()
  // So instead of writing 'new Cons(x, xs)' you can write 'x :: xs'

  // There's a convention for all operators ending in : that they're right associative
  // rather than being left associative (which our operators usually are).
  // So we can omit the paranthesis hell we've had to endure while constructing list3
  val list4 = 0 :: 1 :: 2 :: 3 :: 4 :: 5 :: Nil
                                                  //> list4  : List[Int] = List(0, 1, 2, 3, 4, 5)
  // If we try to look at this from a method call perspective
  // it would be equivalent to method calls of right-hand operands like below
  val list5 = Nil.::(5).::(4).::(3).::(2).::(1)
                                                  //> list5  : List[Int] = List(1, 2, 3, 4, 5)

  // There are three fundamental operations on lists.
  // All other operations can be expressed in terms of these
  // 1) head: First element of the list.
  // 2) tail: The list composed of all elements except 'head'.
  // 3) isEmpty: 'true' if empty, 'false' otherwise.

  // It is possible and preferred to decompose lists as pattern matching
  // Here are some example patterns which can be used
  // 1) Nil pattern
  // 2) p :: ps, A pattern that matches a list with head matching p
  //             and tail matching ps
  // 3) List(p1, ..., pn) is same as p1 :: p2 :: ... :: pn :: Nil

  // Let's do an exercise: Insertion Sort
  // Helper method inserts x in the correct position in the list
  def insert(x: Int, xs: List[Int]): List[Int] = xs match {
    case List() => List(x)
    case y :: ys => if (x > y) y :: insert(x, ys) else x :: xs
  }                                         //> insert: (x: Int, xs: List[Int])List[Int]

  // Main wrapper list decomposer
  def insertionSort(xs: List[Int]): List[Int] = xs match {
    case List() => List()
    case y :: ys => insert(y, insertionSort(ys))
  }                                         //> insertionSort: (xs: List[Int])List[Int]
  // Test out the insertion sort example
  insertionSort(List(5, 4, 3, 2, 1))        //> res1: List[Int] = List(1, 2, 3, 4, 5)
  insertionSort(List(2, 3, 0, 9, 5))        //> res2: List[Int] = List(0, 2, 3, 5, 9)
}
