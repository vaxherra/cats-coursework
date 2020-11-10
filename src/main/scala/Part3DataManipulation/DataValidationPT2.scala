package Part3DataManipulation

import scala.util.Try

object DataValidationPT2 extends App{
  // Validated instances can be chained with something similar to 'flatMap' -> andThen

  import cats.data.Validated
  import cats.Semigroup
  import cats.instances.list._
  implicit val combineIntMax : Semigroup[Int] = Semigroup.instance(Math.max)


  val anInvalidValue : Validated[String,Int] = Validated.invalid("Invalid") // LEFT
  val aValidValue : Validated[String,Int] = Validated.valid(5) // RIGHT

  // if the original value "someValidValue" is invalid, then there are no more transformations chained
  // and the result is NOT ACCUMULATED,
  // VALIDATED IS NOT APPLICABLE TO A MONAD, AS ITS flatMap will short-circuit additional chains ... (expanded on later)

  aValidValue.andThen(someValidValue => anInvalidValue)

  // ensure a value:
  // first parameter list -> in case of an invalid, what to return
  // second parameter list -> the predicate on the "RIGHT" type
  aValidValue.ensure(List("I got an invalid value"))(_ % 2 ==0)

  // transforming validated instances
  aValidValue.map(someInt => someInt+1)

  // transforming the error types
  aValidValue.leftMap(leftValue => leftValue.length)

  // mapping both LEFT and RIGHT types at once
  aValidValue.bimap(leftVal => leftVal.length, rightVal => rightVal+10) /// ERROR TYPE func, VALID VALUE func

  // interoperate with Scala Standard Library: Either, Option, Try
  val eitherToValidated : Validated[List[String], Int] = Validated.fromEither(Right(42))
  val optionToValidated : Validated[List[String],Int] =  Validated.fromOption(None, ifNone =  List("I got None option"))
  val tryToValidated : Validated[Throwable,Int] = Validated.fromTry(Try("something".toInt)) // will be throwable


  // Validated has also backwards API, to turn Validated into Option, Either, Try ...
  aValidValue.toOption
  aValidValue.toEither
  // no backwards compatibility with Try


  // Syntax extension methods for validated
  import cats.syntax.validated._

  val someValid: Validated[List[String],Int] = 42.valid[List[String]]
  val anError : Validated[String,Int] = "Somethings is wrong".invalid[Int]
}
