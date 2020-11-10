package exercises.Part4TypeClasses

object CatsSemigroupal extends App{
  /*
  TODO #1
    Implement the product like function that will return a tuple of A,B generics.
    def productWithMonads[F[_],A,B](fa:F[A], fb:F[B])(implicit monad: Monad[F]): F[(A,B)] = ???
   */
  import cats.Monad
  import cats.Semigroupal
  import cats.instances.list._

  import cats.syntax.flatMap._
  import cats.syntax.functor._


  def productWithMonads[F[_],A,B](fa:F[A], fb:F[B])(implicit monad: Monad[F]): F[(A,B)] = {
    //monad.flatMap(fa)(a => monad.map(fb)(b => (a,b)) ) // equivalent with flatMap and map
    for { // for (still translates to map and flatMap under the hood
      a <- fa
      b <- fb
    } yield (a,b)
  }

  // ====================================================================================================
  /*
      TODO #2: Define a Semigroupal[List] that does not do a cartesian product, buy a ZIP.
   */
  //----------------------------------------------------------------------------------------------------

  // I'll go straight to higher kinded types
  val zipListSemigroupal : Semigroupal[List] = new Semigroupal[List] {
    override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] = fa.zip(fb)
  }

  println(
    zipListSemigroupal.product( List("a","b","c"), List(1,2))
  )













}
