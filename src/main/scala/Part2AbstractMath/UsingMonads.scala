package Part2AbstractMath

object UsingMonads extends App{
  /*

   */

  import cats.Monad
  import cats.instances.list._

  // recap of working with Monads (for example for a List)
  val monadList = Monad[List] // fetches the implicit Monad of type List
  val aSimpleList = monadList.pure(2)
  val anExtendedList = monadList.flatMap(aSimpleList)(x => List(x-1,x,x+1))


  // Either as a MONAD
  val aManualEither  : Either[String,Int] = Right(42)

  // oftentimes we define types for convenience, and they themselves "wrap" some value "T"
  type LoadingOr[T] = Either[String,T] // T is the desired value, String is some form of message
  type ErrorOr[T]  = Either[Throwable,T]

  // and this looks familiar to monads; Either is also a Monad

  import cats.instances.either._
  val loadingMonad = Monad[LoadingOr] // monads of custom types
  val anEither = loadingMonad.pure(42) // LoadingOr[Int] <==> Right(42)
  val aChaingedLoading = loadingMonad.flatMap(anEither)(x => if(x>=1) loadingMonad.pure(x+10) else Left("value smaller than 1") )
  println(aChaingedLoading)


  /*
  an imaginary store, and we track the order status in this store
   */

  case class OrderStatus(orderId: Long, status:String)


  def getOrderStatus(orderId:Long) : LoadingOr[OrderStatus]  = {
    Right(  OrderStatus(orderId,"some order status")  )
  }//implementation not important, however: left if fail, right fetches the status

  def trackLocation(orderStatus:OrderStatus): LoadingOr[String] = {
    // if the data is available - return string, if the data is not present, then return Left("some message")
    if(orderStatus.orderId > 1000) Left("Not available yet, refreshing data")
    else Right("It is in NYC")
    // implementation is not important here, only signatures
  }

  // if we were to track an order location for a particular order ID, we'd need to combine the results of both APIS (methods above)
  // this is really where monad shines;
  val orderId = 457L // long

  // combining calls in SEQUENCE (with monadic flatMap)
  // we can have multiple, yet equally resultant "locations" we want to obtain;
  val orderLocation1 = loadingMonad.flatMap(getOrderStatus(orderId))(someOrderStatus => trackLocation(someOrderStatus))
  val orderLocation2 = loadingMonad.flatMap(getOrderStatus(orderId))(trackLocation) // with LAMBDA

  // or extension methods to use the for expression
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  val orderLocation3 = for {
    someOrderStatus <- getOrderStatus(orderId)
    location <- trackLocation(someOrderStatus)
  } yield location

  val orderLocation4 = getOrderStatus(orderId).flatMap(trackLocation)


}
