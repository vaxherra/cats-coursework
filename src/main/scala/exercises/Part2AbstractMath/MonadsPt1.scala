package exercises.Part2AbstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object MonadsPt1 extends App{
  //====================================================================================================
  // TODO #1.1: warmup exercise
  /*
        How would you create all the combinations of the two lists defined above?
   */
  val numList = List(1,2,3)
  val charList = List("a","b","c")


  // --------------- solution 1.1 b
  val allCombinations = for {
    num <- numList
    char <- charList
  } yield (num,char)
  println(allCombinations)

  // solution 1.1 b
  val allCombinations2 = numList.flatMap(num => charList.map(char => (num,char)))
  println(allCombinations2)

  println(allCombinations==allCombinations2) // true, as for is rewritten by the compiler to the chains of flatMap and map

  // ------------------------------------------------------------------------------------------------------------------
  // TODO #1.2: warmup exercise
  // a combination of num and character from two options (below)

  val aNumOption = Option(2)
  val charOption = Option("d")

  // ----------------------------------------------------------------- #1.2 solution
  // we could do pattern matching, but maps and flatMaps seem better:

  val anOptionOfNumChar = aNumOption.flatMap(num => charOption.map(char => (num,char)))
  println(anOptionOfNumChar)
  // of course this can be also re-written into for loop

  // the same things with futures....
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val aFuture = Future(5)
  val aFuture2 = Future("E")

  // TODO #1.3 warmup
  val aCombinationFuture = aFuture.flatMap(a => aFuture2.map(b => (a,b)))
  Thread.sleep(100)
  println(aCombinationFuture)
  //====================================================================================================
  // the general pattern:
  /*
  1. Wrapping a value with a wrapper, like List, Option, Future
  2. Transforming these values with flatMap/map
   */

  import cats.Monad
  import cats.instances.future._

  val aMonadOfFuture = Monad[Future]
  val someFuture : Future[String] =  aMonadOfFuture.pure("A")
  val someTransformedTurue = aMonadOfFuture.flatMap(someFuture)(x => Future("this is the future "+x))
  Thread.sleep(100)
  println(someTransformedTurue)

}
