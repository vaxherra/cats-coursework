package exercises.Part4TypeClasses



object TraversingPT2 extends App{

    /*
    TODO #1:
      filterAsOption method given, what is the result of the expression provided below
     */
  // importing TraversingPT1 exercises codes
  import exercises.Part4TypeClasses.TraversingPT1.listTraverseBetter
  import cats.instances.option._ //
  import cats.syntax.apply._

  def filterAsOption(list:List[Int])(predicate: Int => Boolean): Option[List[Int]] = {
    // equivalent of "forAll" method
    //listTraverseBetter[Option, Int,Int](list)( n => if (predicate(n)) Some(n) else None )
    listTraverseBetter[Option, Int,Int](list)( n => Some(n).filter(predicate) ) // equivalent of the above
  }

  /*
  TODO #1: what is the result of the following expressions?
   */

  val expr1 = filterAsOption(List(2,4,6))( _ %2 == 0 ) // predicate passes for all of them
  val expr2 = filterAsOption(List(1,2,3))( _ %2 == 0 )

  /*
  predictions:
  expr1 = Some(List(2,4,6))
  expr2 = None
   */

  println(expr1)
  println(expr2)
  // correct!
}
