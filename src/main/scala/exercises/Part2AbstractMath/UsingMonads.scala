package exercises.Part2AbstractMath

object UsingMonads extends App{
  // TODO #1
  /*
  given THE SERVICE LAYER API of a web application
   */

  // GIVEN:
  case class Connection(host:String, port:String)

  val config = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  trait HttpService[M[_]]{
    def getConnection(cfg: Map[String,String]) : M[Connection]
    def issueRequest(connection: Connection, payLoad: String ): M[String]
  }

  /*
  requirements
      - if the provided host and port are found in the GIVEN configuration map, then we'll return
      an M containing a connection with those values
      otherwise the method will fail, according to the logic of the type M
      (for Try it will return a Failure, for Option it will return None, for Future it will be a failed Future, for Either it will return a Left)

    - the issueRequest method returns an M containing the string: "request (payload) has been accepted", if the payload is less than 20 characters
      otherwise the method will fail, according to the logic of the type M
   */

  /*
  TODO: PROVIDE A REAL IMPLEMENTATION OF HTTP SERVICE WITH TRY/OPTION/FUTURE/EITHER... (pick one)

   */

  /// ----------------------------- SOLUTION for Option
  import cats.Monad
  import cats.instances.either._
  import cats.instances.option._
  import cats.instances.string._
  import cats.syntax.either._


  object OptionHttpService extends HttpService[Option]{
    override def getConnection(cfg: Map[String, String]): Option[Connection] = {
      // extracts host and port from provided maps, and yields a connection
      for {
        h <- cfg.get("host") // get returns an Option
        p <- cfg.get("port")  // ...
      } yield Connection(h,p)
    }

    override def issueRequest(connection: Connection, payLoad: String): Option[String] = {
      if(payLoad.length < 20) Some(s"request (${payLoad}) has been accepted")
      else None
    }
  }


  // some self-provided test
  val responseOption1 = OptionHttpService.getConnection(config).
    flatMap(OptionHttpService.issueRequest(_,"Test"))
  println(responseOption1)

  // or equivalently with for expression
  val responseOption2 = for {
    c <- OptionHttpService.getConnection(config)
    r <- OptionHttpService.issueRequest(c, "Test")
  } yield r

  println(responseOption2)

  // =====================================================================================
  // TODO #2 try using the trait with Either

  type ErrorOr[T] = Either[Throwable,T]

  // ------------------------------------------------------- solution
  object ErrorOrHttpService extends HttpService[ErrorOr]{

    override def getConnection(cfg: Map[String, String]): ErrorOr[Connection] = {
      // if the provided cfg contains "host" and "port" keys, then return a Right of connection
      if(cfg.contains("host") && cfg.contains("port")) Right(Connection(cfg("host"),cfg("port")))
      else Left(new RuntimeException("Could not find host and port pair"))
    }

    override def issueRequest(connection: Connection, payLoad: String): ErrorOr[String] = {
      println("      > in issueRequestMethod")
      if(payLoad.length < 20) Right(s"request (${payLoad}) has been accepted")
      else Left(new RuntimeException(s"${payLoad} not accepted"))
    }
  }

  val Invalidconfig = Map(
    "hostess" -> "localhost",
    "porter" -> "4040"
  )
  println("---- separator")
  val errorOrResponde : ErrorOr[String] = for {
    conn <- ErrorOrHttpService.getConnection(Invalidconfig)//ErrorOrHttpService.getConnection(config)
    // for 'InvalidConfig' the below is NOT even executed!
    response <- ErrorOrHttpService.issueRequest(conn,"Test")
  } yield response

  println(errorOrResponde)

  errorOrResponde match {
    case Left(x) => println(s"Something went wrong! Error: $x")
    case Right(x) => println(s"Alright, success, we go $x")
  }


  /// -------------------------
  // for using flatMap and map on any (our defined) data structures;
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  // a nice wrapper with implicit monad;
  def getResponse[M[_]](service : HttpService[M], payload:String)(implicit monad: Monad[M]) : M[String] = {
    for {
      connection <- service.getConnection(cfg = config)
      response <- service.issueRequest(connection, payload)
    } yield response
  }

  println("---------- user friendly API calls")
  // user friendly API: regardless of type of HttpService, we can use the same function
  println(
    getResponse(service = OptionHttpService, payload = "Hello Option "),"\n\n",
    getResponse(service = ErrorOrHttpService, payload = "Hello either")
  )

}
