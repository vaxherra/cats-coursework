package Part4TypeClasses

// importing functions from Exercises in TraversingPT1

import exercises.Part4TypeClasses.TraversingPT1.listTraverseBetter



object TraversingPT2 extends App{
  import cats.data.Validated // there is an instance of Applicative[Validated], but NOT Monad[Validated]
  import cats.instances.list._ // Semigroup[List] => Applicative[ErrorsOr]

  type ErrorsOr[T] = Validated[List[String],T]

  def filterAsValidated(list: List[Int])(predicate: Int => Boolean) : ErrorsOr[List[Int]] = {
    listTraverseBetter[ErrorsOr, Int, Int](list){ n=>
      if (predicate(n)) Validated.valid(n)
      else Validated.invalid(List(s"Predicate for $n failed"))
    }
  }

  val allTrueValidated = filterAsValidated(List(2,4,6))(_%2==0)
  val someFalseValidated = filterAsValidated(List(1,2,3))(_%2==0)

  println(allTrueValidated) // Valid( List(2,4,6) )
  println(someFalseValidated) // Invalid( List("Predicate for 1 failed", "Predicate for 3 failed" )

  /*
  we can obtain an instance of Validated, providing the reaons why it failed (if failed) or the desired type
   */
  // --------------------------------------------------
  import cats.Applicative
  import cats.Functor
  import cats.Foldable
  import cats.Id
  import cats.syntax.applicative._
  import cats.syntax.apply._


  trait MyTraverse[L[_]] extends Foldable[L] with Functor[L] { // as listTraverseBetter uses foldLeft in the implementations we extend it with Foldable
    // also listTraverseBetter implies the existence of an empty container, so it leaves the implementation to the various Type Class Instances
    // based on listTraverseBetter signature, from exercises TraversingPT1.scala
    def traverse[F[_] : Applicative, A,B](container :L[A])(f: A=> F[B]): F[L[B]]

    // similar to listSequence from exercises TraversingPT1.scala,
    // and its implementation is based on traverse
    def sequence[F[_] : Applicative, A ]( container :  L[F[A]]): F[L[A]] = { // turns the data structure "inside-out"
    traverse(container)(identity)
    }

    type Identity[T] = T // corresponds to import cats.Id (identity)
    //def map[A,B](wa : L[A])(f: A=>B): L[B] = traverse(wa)(a => f(a).pure[Identity])
    //def map[A,B](wa : L[A])(f: A=>B): L[B] = traverse[Identity,A,B](wa)(a => f(a).pure[Identity])
    //def map[A,B](wa : L[A])(f: A=>B): L[B] = traverse[Identity,A,B](wa)(f)
    def map[A,B](wa : L[A])(f: A=>B): L[B] = traverse[Id,A,B](wa)(f)
    /*
    Take home message: the TRAVERSE trait can naturally implement the MAP method that is specific to a FUNCTOR,
    and so we can mix in the FUNCTOR TRAIT, and it will be auto-implemented (no need to explicitly override)
     */
  }


  // ====================================================================================================
  /*
  CATS TRAVERSE:
    Now that the method were understood, we'll uncover the cats API for TRAVERSE
   */

  // importing Future's and creating an implicit execution context (implicit val ec)
  import scala.concurrent.{Future,ExecutionContext}
  import cats.instances.future._ // implicit Applicative[Future]
  import java.util.concurrent.Executors
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  // functions COPIED from lectures TraversingPT1.scala
  val servers : List[String] = List("server1-ci.com", "server2-staging.com", "server3-prod.com")
  def getBandwidth(hostname:String) : Future[Int] = Future(hostname.length*80)  // implementation is not important

  import cats.Traverse
  val allBandwidthsCats = Traverse[List].traverse(servers)(getBandwidth)
  (allBandwidthsCats).foreach(println)


  // ===================================================================== Traverse Type Class (TC) extension methods:
  import cats.syntax.traverse._ // sequence + traverse methods
  val allBandwidthsCats2 = servers.traverse(getBandwidth)

  /*
  TRAVERSE USE CASES:
    - turning nested data structures "inside-out":
    - general data combination APIs
   */
  /*
    THE UPDATED TYPE CLASS HIERARCHY:


      FOLDABLE         FUNCTOR         SEMIGROUPAL
            \        /        \        /
              TRAVERSE           APPLY
                               /         \
                        FlatMap           APPLICATIVE
                                \        /           \
                                   MONAD           APPLICATIVE ERROR
                                        \          /
                                          MONAD ERROR

  II.

      SEMIGROUP
          |
        MONOID

   */


}
