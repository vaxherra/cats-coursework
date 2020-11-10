package exercises.Part2AbstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

// CATS IMPORTS
import cats.data.EitherT
import cats.instances.either._
import cats.instances.future._


object MonadTransformers extends App{
 /*
  TODO EXERCISE #1
  SCENARIO:
    Given a multi-machine cluster for some business, which will receive a huge traffic increase given some event.
    We measure the bandwidth in units
    We want to allocate TWO of our servers to cope with the traffic spike.
    We know that the current capacity for each server and we know we'll hold the traffic if the sum of bandwidths is > 250


  */
  // FUNCTIONS GIVEN
 implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(1))

  val expectedSurge : Int = 250

  val bandwidths = Map(
    "server1" -> 50,
    "server2" -> 300,
    "server3" -> 170,
    "server4" -> 68,
  )

  type AsyncResponse[T] = EitherT[Future, String, T] // T is the desired value (Right)

  def getBandwidth(serverName:String ): AsyncResponse[Int] = bandwidths.get(serverName) match {
    // bandwidths get returns an Option of Int, so we pattern match
    //case None => EitherT(Future[Either[String,Int]](Left(s"Unreachable server: $serverName"))) // unreachable server
    //case Some(x) => EitherT(   Future[Either[String,Int]](Right(x))    )

    // but this boilerplate is horrible, better to re-write it as

    case None => EitherT.left(Future(s"Unreachable server $serverName")) // EitherT wrapper over Future[Left[String]]
    case Some(x) => EitherT.right(Future(x)) // EitherT wrapper over  Future[Right[Int]]

  }


  // TODO #1
/*

  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = {
    // returns True if these two servers can withstand the traffic surge (bandwidths are bigger than
    ???
  }

  // TODO #2
  def generateTrafficSurgeReport(s1:String,s2:String ) : AsyncResponse[String] = {
    // string as a description if s1+s2 can cope with traffic spike
  }
*/

  // ------------------------- SOLUTION:

  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = for {
    // returns True if these two servers can withstand the traffic surge (bandwidths are bigger than

    band1 <- getBandwidth(s1)
    band2 <- getBandwidth(s2)

  } yield band1+band2>=expectedSurge // EitherT(Future(Right(band1+band2>=expectedSurge)))


  def generateTrafficSurgeReport(s1:String,s2:String ) : AsyncResponse[String] = {
    // string as a description if s1+s2 can cope with traffic spike
    canWithstandSurge(s1,s2).transform[String,String]({
        // x => match {  // lambda function
      case Left(x) => Left(s"One or all of the servers: $s1 and $s2 are unreachable, reason: $x")
      case Right(false) => Left(s" Servers $s1 and $s2 are reachable, but cannot withstand incoming surge of $expectedSurge")
      case Right(true) => Right(s"Servers $s1 and $s2 are reachable and can withstand the incoming surge of $expectedSurge")
    })
  }

  // SOME TESTS:
  generateTrafficSurgeReport("server1","server3").value.foreach(println) // Servers exist, but cannot cope
  generateTrafficSurgeReport("server11","server3").value.foreach(println) // Server11 does not exist
  generateTrafficSurgeReport("server2","server3").value.foreach(println) // Server11 does not exist


}
