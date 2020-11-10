package exercises.Part5Advanced

object Kleislis extends App{

  /*
  TODO EXERCISE #1 (conceptual exercise):
    What does the pattern of "InterestingKleisli" remind you of in Scala/Cats libraries?

   */

  import cats.data.Kleisli
  import cats.Id

  type InterestingKleisli[A,B] = Kleisli[Id, A, B] // A => Id[B], i.e. A=>B

  val timesTwo = Kleisli[Id,Int,Int](x => x*2)
  val plus4 = Kleisli[Id,Int,Int](y => y+4)

  val composed = timesTwo.flatMap( t2 => plus4.map(p4 => t2+p4)) // chain of maps, flatMaps... so we can use for
  val composedFor = for {
    t2 <- timesTwo
    p4 <- plus4
  } yield t2+p4

  println(composedFor(3)) // "3' is fed into "timesTwo" and also to "plus4" and combined with +, so...
  // (3*2) + (3+4)  = 13

  // ------------------------------------------------------------------- solution:
  /*
  IT is similar to the DEPENDENCY INJECTION pattern, with scala cats it is : A READER
   */

  import cats.data.Reader
  val timesTwoReader = Reader[Int,Int](x => x*2)
  val plus4Reader = Reader[Int,Int](y => y+4)

  // Kleisli[Id,A,B] = Reader[A,B]

}
