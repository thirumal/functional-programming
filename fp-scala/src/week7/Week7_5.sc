package week7

object Week7_5 {
  println("Case Study - Water Pouring Problem")   //> Case Study - Water Pouring Problem
  
  // Please take a look at the Pouring.scala class for implementation details.
 
  //Unit test
  val problem = new Pouring(Vector(4, 9))         //> problem  : week7.Pouring = week7.Pouring@6537cf78
  problem.solution(6)                             //> res0: Stream[week7.Week7_5.problem.Path] = Stream(Fill(1) Pour(1,0) Empty(0)
                                                  //|  Pour(1,0) Empty(0) Pour(1,0) Fill(1) Pour(1,0) --> Vector(4, 6), ?)
}