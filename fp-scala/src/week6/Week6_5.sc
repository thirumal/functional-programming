package week6

object Week6_5 {
  println("Implementation of for-expressions")    //> Implementation of for-expressions
  // How are for-expressions implemented
  //
  // The syntax for for is closely related to the
  // higher order functions map, flatMap and filter
  //
  // First thing to note, that all three functions can all be defined in terms of for
  def mapFn[T, U](xs: List[T], f: T => U): List[U] = {
    for (x <- xs) yield f(x)
  }                                               //> mapFn: [T, U](xs: List[T], f: T => U)List[U]
  def flatMapFn[T, U](xs: List[T], f: T=> Iterable[U]): List[U] = {
    for (x <- xs; y <- f(x)) yield y
  }                                               //> flatMapFn: [T, U](xs: List[T], f: T => Iterable[U])List[U]
  def filterFn[T](xs: List[T], p: T => Boolean): List[T] = {
    for (x <- xs if (p(x))) yield x
  }                                               //> filterFn: [T](xs: List[T], p: T => Boolean)List[T]
  // In reality, things go the other way
  // Scala compiler actually translates the for expression into a combination
  // of map, flatMap and a lazy variant of filter.

  // A simple for expression consisting of only a a generator
  // for(x <- e1) yield e2
  // is translated into
  // e1.map(x => e2)

  // Another for expression consisting of a generator and a filter
  // for (x <- e1; if f; s) yield e2
  // the above can be re-written into
  // for (x <- e1.withFilter(x => f); s) yield e2
  // and the translation continues with the new expression.
  // 'withFilter' is a variant of 'filter' that does not produce
  // an intermediate list, but instead filters the following map
  // or flatMap function application.

  // The last kind of for-expression we need to cover is of the below:
  // for (x <- e1; y <- e2; s) yield e3
  // this translates into
  // for e1.flatMap(x => for (y <- e2; s) yield e3)
  // and the translation continues...

  // Translate the for-expression from the previous class
  // for (b <- books; a <- b.authors if a startsWith "Bird") yield b.title
  // Step #1:
  // books.flatMap(b => { for(a <- authors if a startsWith "Bird") yield b.title })
  // Step #2:
  // books.flatMap(b => {
  //   for(a <- b.authors.withFilter(a => a.startsWith("Bird")) yield b.title
  // })
  // Step #3 & final step:
  // books.flatMap(b => {
  //   b.authors.withFilter(a => a.startsWith("Bird")).map(y => b.title)
  // })
}