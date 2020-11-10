package Part2AbstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object MonadsPt1 extends App{
  /*
  MONADS :
   */

  // the monad has to implement these TWO basic operations: unit ("pure") and flatMap ...
  trait MyMonad[M[_]]{ // M type is itself Generic (higher kinded type)
    def pure[A](value :A): M[A] //
    def flatMap[A,B](container:M[A])(f: A => M[B]): M[B]
  }

  // but cats has already the API ready:
  import cats.Monad
  import cats.instances.option._

  val aMonadOption = Monad[Option]

  val someOption: Option[Int] = aMonadOption.pure(2)
  val aTransformedOption =  aMonadOption.flatMap(someOption)(x => Some(x+10))
  println(someOption,aTransformedOption)

  import cats.instances.list._
  val listMonad = Monad[List]
  val aList = listMonad.pure(3)
  val aTransformedList = listMonad.flatMap(aList)(x => List(x-2,x-1,x,x+1,x+2))
  println(aTransformedList)

  //=====================================================================================================
  // specialized API
  def getPairsList(numbers : List[Int], chars: List[Char]): List[(Int,Char)] =  numbers.flatMap(n => chars.map(c => (n,c)))
  def getPairsMonad(numbers : Option[Int], chars: Option[Char]): Option[(Int,Char)] =  numbers.flatMap(n => chars.map(c => (n,c)))
  // and so on if we want to support Futures, or custom types...
  // but we DUPLICATE API! This is not good...

  // so a MONAD comes and saves the day,
  // we additionally define an implicit parameter - a monad of higher kinded Type
  def getPairs[M[_], A, B]( ma: M[A], mb: M[B])(implicit monad : Monad[M]): M[(A,B)] = {
    // ma corresponds to the above "numbers", and mb to the above "chars"
    monad.flatMap(ma)( a => monad.map(mb)(b => (a,b))   )
  }

  // the above IS VERY GENERAL!

  // define some dummy data of various types
  val numList = List(1,2,3)
  val charList = List("a","b","c")
  val aNumOption = Option(2)
  val charOption = Option("d")
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val aFuture = Future(5)
  val aFuture2 = Future("E")


  println("---- a general MONAD API test")
  println(
    getPairs(numList,charList)
  )

  println(
    getPairs(aNumOption,charOption)
  )

  import cats.instances.future._
  getPairs(aFuture,aFuture2).foreach(println)

  // check the exercises for MonadPt1
  // we can then TRANSFORM ANY TYPE OF MONADIC VALUE IF WE HAVE AN IMPLICIT MONAD OF THAT TYPE IN SCOPE
}
