package Part2AbstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object MonadsPt2 extends App{
  import cats.Monad
  import cats.instances.option._
  import cats.instances.list._

  //--------------------------------------------------------- MONADS EXTENSION METHODS -- part of some weirder imports
  /*
  pure and flatMap are its extension methods
   */
  import cats.syntax.applicative._
  val oneOption = 1.pure[Option] // Some(1)
  val oneList = 1.pure[List]

  // for flatMap
  import cats.syntax.flatMap._
  val oneOptionTransformed = oneOption.flatMap(x => (x+10).pure[Option])
  println(oneOption,oneOptionTransformed)

  // MONADS EXTEND FUNCTORS, and hence we can:
  import cats.syntax.functor._
  val oneOptionMapped1 = oneOption.map(_+10)
  val oneOptionMapped2 = Monad[Option].map(Option(5))(_+10)


  // we also have access to for comprehensions
  val composedOptionFor = for{
    one <- 1.pure[Option]
    two <- 2.pure[Option]
  } yield one+two

  // check the exercises for MonadsPt2

  /*
  MONADS:
   - higher-kinded type class that provides
   > a PURE method to wrap a normal value into a monadic value
   > a FLATMAP method to transform monadic values in sequence
   */

  /*
  use cases for monads:
  - anything that involves sequential transformations
    > list combinations
    > option transformations
    > asynchronous chained computations
    > dependent computations
   */

  // FOR COMPREHENSIONS ARE NOT AN ITERATION
  // and flatMap is a "mental model" of chained transformations
  
}
