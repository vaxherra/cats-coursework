package Part4TypeClasses

object CatsApplicative extends App{
  /*
  Reminder: Functors provide a map method

  Applicatives are an extension to Functors that will provide the "pure" method, the same "pure" from Monad

  APPLICATIVES == FUNCTORS + PURE METHOD

  reminder : Pure wraps a "normal" value into the "wrapped" value;
   */

  import cats.Applicative
  import cats.instances.list._ // implicit Applicative[List]

  val aListApplicative = Applicative[List]
  val aList = aListApplicative.pure("5")
  println(aList) // List("5")

  import cats.instances.option._ // implicit Applicative[Option]
  val anOptionApplicative = Applicative[Option]
  val anOption = anOptionApplicative.pure(12) // Some(2):Option[Int]

  /*
  Why are applicatives useful? They are functors, so have a MAP method, and can wrap a value with PURE
  - generalizing API
   */

  // "PURE"  EXTENSION METHODS
  import cats.syntax.applicative._
  val aSweetList = 2.pure[List]
  val aSweetOption = 2.pure[Option]
  // ...

  // MONADS EXTEND APPLICATIVES (and SEMIGROUPALS as well)
  // APPLICATIVES EXTEND FUNCTORS
  // APPLICATIVES EXTEND SEMIGROUPALS ( see the exercises for CatsApplicative.scala)

  // most of data structures we use are actually monadic, so applicatives are not used extensively
  // the exception is the Validated Type Class data manipulation tool
  // Validated does not conform to Monad

  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]

  val aValidValue : ErrorsOr[Int] = Validated.valid(5) // "pure" method
  val aModifiedValidated : ErrorsOr[Int] = aValidValue.map(_+10) // "map" method

  val validatedApplicative = Applicative[ErrorsOr]

  /*
  The general use case, is to use applicatives for the data types that are functors;

  APPLICATIVES = FUNCTORS + THE PURE METHOD

  play a role in the Type Class Hierarchy;

  So far, the hierarchy looks like this:

I.

 FUNCTOR              SEMIGROUPAL
        \ APPLICATIVE /
               |
             MONAD

II.

    SEMIGROUP
        |
       MONOID
   */













}
