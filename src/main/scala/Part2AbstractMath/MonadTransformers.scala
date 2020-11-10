package Part2AbstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}


object MonadTransformers extends App{

  /*
  Sometimes we need to operate on the combinations of monadic values, like a list of Options and then nest them together

   */


  // ============== OPTION TRANSFORMER ==================================================================
  def sumAllOptions(values: List[Option[Int]]): Int = ???
  // Monad transformers would allow us to apply map and flatMap without the need to "manually unwrap" these nested 'things'

  import cats.data.OptionT // Option Transformer (EitherT ... )
  import cats.instances.list._ // compiler fetches an implicit Monad[List]

  val aListOfNumOptions : OptionT[List,Int] = OptionT(   List( Option(1), Option(2) )   )
  val aListOfCharOptions : OptionT[List,Char] = OptionT(   List( Option('a'), Option('b'),Option.empty[Char]  )    )

  // combining the list of options chars and nums into some tuples, then:
  // we'd need to unwrap every option and wrap them back... clunky, impractical

  // OptionT has map and flatMap methods, so we can easily say
  val aListOfTuples : OptionT[List, (Int,Char)] = for {
    char <- aListOfCharOptions
    num <- aListOfNumOptions
  } yield (num,char)

  println("\nOption transformer -----------------")
  println(aListOfTuples)
  println(aListOfTuples.value)

  // ============== EITHER TRANSFORMER ================================================================================
  import cats.data.EitherT

  println("\nEither transformer -----------------")
  val aListOfEithers : EitherT[List,String,Int] = EitherT(   List(Left("Something wrong"),Right(42),Right(621))   )
  println(aListOfEithers.value)

  //====================================================================================================================
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

  val aFutureOfRight : Future[Either[String,Int]] = Future(Right(42)) // enforce correct type
  val aFutureOfEither : EitherT[Future, String,Int] = EitherT (   aFutureOfRight   )

}
