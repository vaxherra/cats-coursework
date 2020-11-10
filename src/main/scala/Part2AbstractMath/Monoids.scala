package Part2AbstractMath

object Monoids extends App{
  /*
  MONOIDS:
   */

  // comparing Semigroup to a monoid: where does Semigroup fail, where Monoid can take over?
  import cats.Semigroup
  import cats.instances.int._
  import cats.instances.string._
  import cats.instances.option._
  import cats.syntax.semigroup._ // |+| extension method of combine


  val numbers = (1 to 1000).toList
  // the combine method is ASSOCIATIVE |+|
  // i.e. the order does not matter (A+B)+C = A+(B+C)

  val sumNumbersLeft = numbers.foldLeft(0)( _ |+| _ )
  val sumNumbersRight = numbers.foldRight(0)( _ |+| _ )
  println(sumNumbersLeft==sumNumbersRight)


  // defining a general API
  /*
    def combineFold[T](list:List[T])(implicit  semigroup: Semigroup[T]): T  =  {
      list.foldLeft(0)(_ |+| _)
      // what would be the starting value for generic Type T? There is no simple answer; if we knew the type T,
      // but not in general
    }
  */

  // ============================================================================================================
  // Empty value / Neutral value  -> MONOID PROVIDES default values
  import cats.Monoid

  val intMonoid = Monoid[Int]
  // monoid can combine values
  // a monoid with a default value
  val zero = intMonoid.empty // default 0
  val combineInt = intMonoid.combine(1,2) // a similar API as compared to Semigroup

  println(combineInt)

  val emptyString = Monoid[String].empty // the natural NEUTRAL ELEMENT is the empty string ""
  val combineString = Monoid[String].combine("Monoids are", " fun!")

  // when looking up the Monoid sourc code (cmd+click in intellij) you can see that
  // MONOID EXTENDS SEMIGROUP WITH ADDITIONAL DEF EMPTY (and others)

  // option
  val emptyIntOption = Monoid[Option[Int]].empty // None
  val combineIntOption = Monoid[Option[Int]].combine(Option(2), Option.empty[Int])
  println(combineIntOption)

  // monoid have the same combine syntax sugar |+|
  val combinedOptionsFancy = Option(1) |+| Option(10)
  // notice that we DID NOT import syntax for monoid, but use the syntax for semigroup
  // because monoid extends semigroup, this works perfectly
  // you can import it explicitly
  import cats.syntax.monoid._
  println(combinedOptionsFancy)

/*
MONOID USE CASES:

  - BIG DATA PROCESSING,
  - DATA INTEGRATION
  - EVENTUAL CONSISTENCY, and timestamp reconciliation
  - DISTRIBUTED COMPUTING
 */


}
