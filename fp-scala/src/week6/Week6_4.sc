package week6

object Week6_4 {
  println("Queries with for-expressions")         //> Queries with for-expressions
  // Exploring the relationship between for-expressions
  // and database query languages

  // Suppose we have a database of books, represented as a list
  // of books
  case class Book(title: String, authors: List[String])
  // Mini database on which we run queries on:
  val books: Set[Book] = Set(
	  Book(title = "Structure and Interpretation of Computer Programs", authors = List("Abelson, Harald", "Sussman, Gerald J.")),
	  Book(title = "Introduction to Functional Programming", authors = List("Bird, Richard", "Wadler, Phil")),
	  Book(title = "Effective Java", authors = List("Bloch, Joshua")),
	  Book(title = "Effective Java 2", authors = List("Bloch, Joshua")),
	  Book(title = "Java Puzzlers", authors = List("Bloch, Joshua", "Gafter, Neal")),
	  Book(title = "Programming in Scala", authors = List("Odersky, Martin", "Spoon, Lex", "Venners, Bill"))
  )                                               //> books  : Set[week6.Week6_4.Book] = Set(Book(Effective Java 2,List(Bloch, Jos
                                                  //| hua)), Book(Programming in Scala,List(Odersky, Martin, Spoon, Lex, Venners, 
                                                  //| Bill)), Book(Structure and Interpretation of Computer Programs,List(Abelson,
                                                  //|  Harald, Sussman, Gerald J.)), Book(Effective Java,List(Bloch, Joshua)), Boo
                                                  //| k(Introduction to Functional Programming,List(Bird, Richard, Wadler, Phil)),
                                                  //|  Book(Java Puzzlers,List(Bloch, Joshua, Gafter, Neal)))
  // Let's run some queries on these books
  // 1) Find the titles of the books whose author's name is Bird
  for {
    book <- books
    author <- book.authors
    if (author startsWith "Bird,")
  } yield book.title                              //> res0: scala.collection.immutable.Set[String] = Set(Introduction to Function
                                                  //| al Programming)
  // Find all the books which have the word "Program" in the title
  for {
    book <- books
    if ((book.title).indexOf("Program") >= 0)
  } yield book.title                              //> res1: scala.collection.immutable.Set[String] = Set(Programming in Scala, St
                                                  //| ructure and Interpretation of Computer Programs, Introduction to Functional
                                                  //|  Programming)
  // Find all authors who have written two books in the database
  for {
    b1 <- books
    b2 <- books
    if (b1.title < b2.title) // Lexicographical comparision of titles
    a1 <- b1.authors
    a2 <- b2.authors
    if(a1 == a2)
  } yield a1                                      //> res2: scala.collection.immutable.Set[String] = Set(Bloch, Joshua)
}