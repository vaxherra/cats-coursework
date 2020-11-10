package Part4TypeClasses

object Folding extends App{
  /*
  TODO :See first Exercise #1
   */

  /*
  FOLDABLES ARE VERY USEFUL FOR GENERALIZABLE APIs and for CONVENIENCE;

  In the Type Class Hierarchy, the FOLDABLE does not have any relationship to the previous Type Classes discussed;
   */

  import cats.Foldable // a higher kinded type
  import cats.instances.list._ // implicit Foldable[List]

  //---------------------------------------------------------------------------------------------------- FOLD LEFT
  // Foldable has the fundamental foldLeft/foldRight
  val sum =  { // slightly different (order of elements in) API, than scala standard foldLeft/foldRight
    Foldable[List].foldLeft(List(1, 2, 3), "Starting: ")((someString, someInt) => someString +" "+ someInt.toString+",")
  }

  import cats.instances.option._ // implicit Foldable[Option]
  val sumOption =  {
    Foldable[Option].foldLeft(Option(2), 30)((someInt, someOption) => someInt + someOption)
  }

  //---------------------------------------------------------------------------------------------------- FOLD RIGHT
  import cats.Eval
  val sumRight = Foldable[List].foldRight( List(1,2,3), Eval.now(0) )( (element, currentEval) => currentEval.map(_+element))
  println(sumRight.value)

  /* STACK SAFETY NOTE:
  This is useful, as foldRight can easily be implemented in terms of STACK RECURSION, and using the EVAL
  makes everything STACK SAFE regardless of how our container is being implemented!
   */


  /*
  FOLDABLES can relay on other Type Classes, like MONOID, to provide some extra functionality;
   */
  import cats.instances.int._ /// Monoid[Int]
  val anothersum = Foldable[List].combineAll(List(1,2,3,4,5,6)) //combines all in the presence of the implicit Monoid[Int]

  import cats.instances.string._ // implicit Monoid[String]
  val mappedConcat = Foldable[List].foldMap(List(1,2,3))(x => x.toString)


  // DEEP TRAVERSALS for multiple data structures nested
  val intsNested = List(Vector(1,2,3), Vector(4,5,6))

  import cats.instances.vector._
  (Foldable[List] compose Foldable[Vector]).combineAll(intsNested)

  // EXTENSION METHODS
  import cats.syntax.foldable._
  val sum3 = List(1,2,3).combineAll // requires Foldable[List], Monoid[Int]

  val mappedConcat2 = List(1,2,3).foldMap(_.toString) // Foldable[List], Monoid[String]



}
