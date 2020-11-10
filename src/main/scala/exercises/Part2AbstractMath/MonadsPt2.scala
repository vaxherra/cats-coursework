package exercises.Part2AbstractMath

import java.util.concurrent.Executors


import scala.concurrent.{ExecutionContext, Future}

object MonadsPt2 extends App{

  /*
  TODO 3

  implement the map method in MyMonad

 trait MyMonad[M[_]]{ // M type is itself Generic (higher kinded type)
    def pure[A](value :A): M[A] //
    def flatMap[A,B](container:M[A])(f: A => M[B]): M[B]
  }

   */

  trait MyMonad[M[_]]{ // M type is itself Generic (higher kinded type)
    def pure[A](value :A): M[A] = ???
    def flatMap[A,B](container:M[A])(f: A => M[B]): M[B] = ???

    // todo #3 solution
    def map[A,B](container:M[A])(f: A=>B): M[B] = flatMap(container)(x => pure(f(x)) )
  }

  // hence the (cats) oly abstract methods for Monad are `pure` and `flatMap`, as `map` is implemented using the previous two;
  // and in that sense Monad extends a Functor!  As Monad provides the fundamental method of a functor: a `map`

  // =====================================================================================================
  /*
  TODO 4: implement a shorter version of get pairs using for comprehension:

   def getPairs[M[_], A, B]( ma: M[A], mb: M[B])(implicit monad : Monad[M]): M[(A,B)] = {
    // ma corresponds to the above "numbers", and mb to the above "chars"
    monad.flatMap(ma)( a => monad.map(mb)(b => (a,b))   )
  }
   */

  import cats.Monad
  import cats.instances.int._
  import cats.instances.option._
  import cats.instances.list._

  import cats.syntax.applicative._
  import cats.syntax.flatMap._ // flatMap extension method
  import cats.syntax.functor._ //map extension method

  def getPairs[M[_] : Monad, A, B]( ma: M[A], mb: M[B]): M[(A,B)] = {
  //def getPairs[M[_], A, B]( ma: M[A], mb: M[B])(implicit monad : Monad[M]): M[(A,B)] = {
    for {
      a <- ma // using flatMap extension method
      b <- mb // using map extension method
    } yield(a,b)
  }

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

  val somePairs =  getPairs(numList,charList)
}
