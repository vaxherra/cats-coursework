package exercises.Part3DataManipulation


import scala.annotation.tailrec

object DataValidationPT1 extends App{
/*
TODO EXERCISE #1
  We want to test if the number meets the conditions:
  1. must be prime
  2. must be non-negative
  3. n<=100
  4. must be EVEN
  (only "2" meets these conditions).
  Implement "def testNumber" with EITHER return type. This method must return all the conditions failed,
  IT WILL ILLUSTRATE WHY "EITHER" IS NOT OPTIMAL, AND WHY SOMETHING LIKE CATS.DATA.VALIDATE IS really good!

 */
  def testNumber_signature(n:Int) : Either[List[String], Int] = ???

  // -----------------------------------------------------------------------------------------------------
  // a helper method:
  def isPrime(n:Int) : Boolean =  {
    @tailrec
    def tailRecPrime(diviser:Int): Boolean = {
    if(diviser<=1) true
    else (n%diviser)!=0 && tailRecPrime(diviser-1)
    }
    if(n<=1) false
    else tailRecPrime(Math.abs(n/2))
  }

  import cats.data.Validated

  def testNumberEither(n:Int) : Either[List[String], Int] = { // VERY BAD CODE ahead (on purpose)
    // this very bad code, is still made look not that bad

    // isolate all individual errors
    val isPrimeTest   : List[String] = if(isPrime(n)) List()  else List(s"Number $n is not a prime number")
    val isNegative: List[String] = if(n>=0) List() else List(s"Number $n is negative")
    val isLessThan100: List[String] = if(n<=100) List() else List(s"Number $n is greater than 100")
    val isNotEven : List[String] = if(n%2==0) List() else List(s"Number $n is not even")

    // test all predicates at once, and combine all lists into one
    if(isPrime(n) && n>=0 && n<=100 && n%2==0) Right(n)
    else Left(isPrimeTest ++ isNegative ++ isLessThan100 ++ isNotEven)
  }



  // now with CATS we can simply COMBINE the VALIDATED instances, as opposed to testing each EITHER SEPARATELY

  //====================================================================================================
  /*
  TODO Exercise #2
      Rewrite the above function in terms of cats.data.Validated
   */

  // the COMBINE need the DEFAULT/IMPLICIT combination function,
  // and we need a semigroup of List and Int

  // for List we're going to import an implicit semigroup...
  import cats.instances.list._
  // for Int we're going to define an implicit value
  import cats.Semigroup
  implicit val combineIntMax : Semigroup[Int] = Semigroup.instance[Int]((a,b)=> Math.max(a,b))


  def validateNumber(n:Int): Validated[List[String],Int] = {
    Validated.cond(isPrime(n), n, List("Number must be prime"))
      .combine(   Validated.cond(n>=0, n, List("Number must be positive"))   )
      .combine(Validated.cond(n<=100,n ,List("Number must be less than or equal to 100")))
      .combine(Validated.cond(n%2==0, n, List("Number n must be even")))
  }

}
