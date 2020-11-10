package Part4TypeClasses

import java.util.concurrent.Executors


import scala.concurrent.{ExecutionContext, Future}

object TraversingPT1 extends App{
  /*
  TRAVERSE: offer higher level approach to the "iteration"
   */

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  // we are pinging some servers in some structure
  val servers : List[String] = List("server1-ci.com", "server2-staging.com", "server3-prod.com")

  // contains a number, which means the total bandwidth
  def getBandwidth(hostname:String) : Future[Int] = Future(hostname.length*80)  // implementation is not important

  // gathering all the bandwidths as a SINGLE FUTURE
  // <<WITHOUT CATS "magic">>, the most elegant method possible
  val allBandwidths : Future[List[Int]] = servers.foldLeft(Future(List.empty[Int]))( (accumulator, hostname ) => {
    val bandFuture : Future[Int] = getBandwidth(hostname)
    // and now compose Futures;
    for {
      accBandwidths <- accumulator
      band <- bandFuture
    } yield accBandwidths :+ band
  })
  /*
  MANUAL SOLUTION:
  overview: we have List[String], then each String is turned into Future[String], and we want to have Future[List[Int]]
  the drawback: we need to define Futures all the time, and unwrap and then wrap them again;
   */


  // a BETTER SOLUTION: elegant way
  val allBandwidthsSequence : Future[List[Int]] =  Future.traverse(servers)(getBandwidth)

  val allBandwidthsSequence2 : Future[List[Int]] = Future.sequence(servers.map(getBandwidth))
  // servers.map(getBandwidth) : List[Future[Int]],
  // and then Future.sequence transforms it into Future[List[Int]]

 //--------------------------------------------------------------------------------------------------------------
  // TODO Exercise #1 (see exercises file)

    // after doing the exercise:
  import cats.{Monad,Applicative}
  import cats.syntax.applicative._ // pure
  import cats.syntax.apply._// mapN

  import cats.syntax.flatMap._ // flatMap
  import cats.syntax.functor._ // for


  // this function has a lowr minimal bound, Applicative >: Monad,
  // we can still combine two elements, but without for comprehension (map, flatMap)
  def listTraverse[F[_] : Applicative, A,B](list:List[A])(f: A=> F[B]): F[List[B]] = {

    list.foldLeft( List.empty[B].pure[F] )( (accMonad, listElem) => {
      val transformedElemWrapped : F[B] = f(listElem)
      // mapN is applied to the tuple of wrappers; and is applied to the values inside
      // F[List[B]], F[B]
      (accMonad, transformedElemWrapped).mapN( _ :+ _ ) // List[B], [B] => some generic Z, but our return type will be List[B],
      // and hence we need take List[B] and append :+ [B]
    })
  }


  // The above proves that the MINIMUM REQUIREMENT to implement this listTraverse method is NOT MONAD, but APPLICATIVE
  // this can be extended for example to VALIDATED that is NOT A MONAD.

  // TODO See Exercise #2 of TraversingPT1.scala

}
