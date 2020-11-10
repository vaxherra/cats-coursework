package Part5Advanced

object Kleislis extends App{
  /*
  What is a KLEISLI?
    KLEISLI is a generic data structure that will help with composing functions that will return wrapper instances

    KLEISLI is a wrapper over functions returning Higher Kinded Instances, and they are useful for combining these functions
    and returning a single value at the end.

    Because they are wrappers, they provide lots of convenience methods:
    - map, flatMap,
    - apply,
    - andThen,
    - traverse

    USED IN PRACTICE FOR:
    - function composition when they return F[_] types
    - dependency injection - the "Reader" type
   */

  val func1: Int => Option[String]    =     x => if(x%2==0) Some(s"$x is even") else None
  val func2: Int => Option[Int]       =     x => Some(3*x)

  // we'd like to be able to compose functions, to say
  // val func3 = func2 andThen func1
  // if they were not wrapped in Options, this would have been easily done with the "andThen" method as above


  // chaining operations that return a wrapper types: KLEISLI
  import cats.data.Kleisli
  import cats.instances.option._ // implicit map/flatMap[Option] that is required;
  // the andThen method applies a flatMap

  val func1Kleisli : Kleisli[Option,Int,String] = Kleisli(func1) // wraps an option from Int to String
  val func2Kleisli : Kleisli[Option,Int, Int]   = Kleisli(func2)
  val func3Kleisli : Kleisli[Option,Int,String] = func2Kleisli andThen func1Kleisli // Int => Option[Int] => Kleisli unwrapped Option[Int] to Int => Option[String]

  // convenience APIs
  val multiply = func2Kleisli.map(_*2) // uses implicit functor of Option; equivalent to Option(x).map(_*2)
  val chain = func2Kleisli.flatMap( x => func1Kleisli )

  // when inspecting the cats.data.Reader (see exercises), you'll find out that it is implemented using Kleisli.
  


}
