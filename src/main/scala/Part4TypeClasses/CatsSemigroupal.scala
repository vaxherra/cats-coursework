package Part4TypeClasses

import java.util.concurrent.Executors

import cats.Semigroup

import scala.concurrent.{ExecutionContext, Future}

object CatsSemigroupal extends App {

  // SEMIGROUPAL IS A TYPE CLASS WITH A SINGLE METHOD, THAT ALLOWS TO GROUP VALUES INTO TUPLES WITH
  // REGARD TO THE WAY THEY WERE COMPUTED
  //

  trait MySemigroupal[F[_]] { // F -  a higher kinded type,
    def product[A,B](fa:F[A], fb:F[B]) : F[(A,B)]
  }

  import cats.Semigroupal
  import cats.instances.option._ // implicit semigroupal of option

  val optionSemigroupal = Semigroupal[Option]

  val aTupledOption = optionSemigroupal.product(Option(5), Option("test"))
  val aTupledOption2 = optionSemigroupal.product(Option(5), None) // result -> None
  println(aTupledOption) // -> Some((5,"test"))

  /// --------------------------------------------
  import cats.instances.future._ // imports implicit Semigroupal[Future]
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  // these both future will run in parallel, and once they return a value, those values will be put
  // into tupled together into a new future.
  val aTupledFuture = Semigroupal[Future].product(Future(5),Future(2L))
  aTupledFuture.foreach(println)


  // --------------------------------------------------------------------------------------------------------------
  /*
  Why are Semigroupals useful at all?
   */
  import cats.instances.list._ // Semigroup[List], Monad[List], Semigroupal[List] ...

  val aTupledList = Semigroupal[List].product( List(1,2), List("a","b") ) // a cartesian product between the two lists;
  println(aTupledList) // but the intuition might say, that we would rather ZIP it?

  // ----------------------------------------------------------------------------------------------------
  /*
    Since the "product" function (see exercises scala file) can be implemented in terms of map & flatMap, i.e. using
    Monad.

    And in that sense MONADS EXTEND SEMIGROUPALS

    and that is why in the example above (with the Semigroupal of List) we see the cartesian product.

    so cats.instances.list._ imports Monad[List] which is also Semigropal[List]

   */

  // Why are Semigroupals useful at all?
  /*
      a use case for Semigropals is Validated; combining instances without needing to follow the Monadic laws
   */

  println("\n --------------\nValidated Version of tracking errors with Semigroupal")
  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String],T]

  val validatedSemigroupal = Semigroupal[ErrorsOr] // Semigroupal[Validated[List[String],T]
  // and this will implement the "Product" of two "Validated" instances in terms of independently combining
  // the error type and the value type, according to their "SEMIGROUP"

  // we don't have to follow monadic laws:
  val invalidsCombination = validatedSemigroupal.product(
    Validated.invalid(List("Something wrong #1", "Something wrong #2")),
    Validated.invalid(List("Something wrong #3"))
  )
  println(invalidsCombination)



  type EitherErrorsOr[T] = Either[List[String],T]
  import cats.instances.either._ // imports an implicit Monad[Either]
  val eitherSemigroupal = Semigroupal[EitherErrorsOr] // implicit Monad of [EitherErrorsOr] type
  val eitherCombination = eitherSemigroupal.product( // product implemented as a chain of map and flatMap
    Left(List("Something wrong #1", "Something wrong #2")), // and FOR THIS REASON, THE LAST ERROR WON'T BE TAKEN INTO ACCOUNT
    Left(List("Something wrong #3")) // <-- this is skipped :(
  )
  println("\n --------------\nEither Version of tracking errors with Semigroupal")
  println(eitherCombination)

  // so this is WHY WE SHOULD NOT USE "EITHER" TO TRACK ERRORS, RATHER USE CATS.DATA.VALIDATED!

  // Semigroupal does not follow the associativity law:
  // m.flatMap(f).flatmap(g) != m.flatMap(x => f(x).flatmap(g) )
  // where Either follows that, yet it is more of a hindrance for our use case;


  /*
  RECAP:
      SEMIGROUPALS are a higher-kinded types that can tuple elements.
      PRODUCT is the fundamental method of a semigroupal

      MONADS extends SEMIGROUPALS, and the PRODUCT METHOD is implemented in terms of flatMap and map

      some semigroupals are useful WITHOUT BEING MONADS, ex. VALIDATED example in the code

      DON'T CONFUSE SEMIGROUP WITH SEMIGROUPAL:
        - semigroup has a combine method with syntax |+|
        - semigroupal has a product method for creating tuples
   */
}
