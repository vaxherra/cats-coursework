package Part2AbstractMath

object SemiGroups extends App{
  // SEMI GROUPS ARE A TYPE CLASS:
  // that provide a combination function between elements
  // -> combining elements of the same type

  import cats.Semigroup
  import cats.instances.int._
  import cats.instances.string._
  import cats.instances.option._

  // --------------------------------------------------------------- Int semigroup
  val aNaturalIntSemiGroup = Semigroup[Int] // fetches implicit TCI
  val intCombination = aNaturalIntSemiGroup.combine(5,12) // an addition as a natural combination of Int
  println(intCombination)

  def reduceInt(a: List[Int]): Int = a.reduce(aNaturalIntSemiGroup.combine)
  val someNumList = (1 to 10).toList
  println(reduceInt(someNumList))

  // --------------------------------------------------------------- String semigroup
  val aNaturalStringSemiGroup = Semigroup[String]
  val stringCombination = aNaturalStringSemiGroup.combine("I love", " Cats!")
  println(stringCombination)

  def reduceString(a: List[String]): String = a.reduce(aNaturalStringSemiGroup.combine)
  val someStrList = List("a","b","c","d","e","f")
  println(reduceString(someStrList))

  //====================================================================================================
  println("------------------- GENERAL SEMIGROUP API")
  // --------------------------------------------------------------- General API: reduce generic type
  def reduceThings[T](a:List[T])(implicit semigroup: Semigroup[T]): T = a.reduce(semigroup.combine)

  println(reduceThings(someNumList))
  println(reduceThings(someStrList))

  // here the power of a semigroup shines, as List of String/Int can be "combined" with a map.
  val numberOptions: List[Option[Int]] = someNumList.map(x => Option(x)) // list of options
  println(reduceThings(numberOptions))

  // TODO: see exercise Semigroups;

  //====================================================================================================
  println("----------------------------------------- CONVENIENCE EXTENSION METHODS")

  import cats.syntax.semigroup._
  // new method   |+|    (the combine method)

  val anIntSum = 2 |+| 3 // requires a presence of Implicit semigroup of Int (import cats.instances.int._ )
  println(anIntSum)

  val aStringConcat = "I am Robert " |+| " Kwapich, nice to meet you."
  println(aStringConcat)


  /*
  SUMMARY:

  SEMIGROUP can combine two values of the same type, Once can import syntax to |+| to combine them, provided
  one uses the implicit type class for that instance

  REAL LIFE USAGE OF SEMIGROUPS:
  - DATA INTEGRATION & BIG DATA PROCESSING;
  - EVENTUAL CONSISTENCY & DISTRIBUTED COMPUTING;


   */



}
