package exercises.Part4TypeClasses
import cats.{Functor, Semigroupal}

object WeakerApplicatives extends App{
  /*
  TODO EXERCISE #1
    For the provided trait MyApply define mapN
   */

  trait MyApply[F[_]] extends Functor[F] with Semigroupal[F] {

    def ap[B, T](ff: F[B => T])(fa: F[B]): F[T] // fundamental to the APPLY trait

    override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
      val functionWrapper: F[B => (A, B)] = map(fa)(a => /* this needs to be a function */ (b: B) => (a, b))
      ap(functionWrapper)(fb)
    }

    // TODO implement mapN in terms of 'ap' and 'product'
    def mapN[A, B, C](tuple: (F[A], F[B]))(f: (A, B) => C): F[C] = {

      val tupleWrapper = product(tuple._1, tuple._2) // F[(A,B)]

      map(tupleWrapper) { // here, we need to provide a function (A,B) => C
        case (a, b) => f(a, b) // applied the function and returns type C
        // the map function, then wraps it into F[C]
      }

    }

  }

  trait MyApplicative[F[_]] extends MyApply[F]  {
    def pure[A](x:A): F[A] // fundamental method of an applicative is PURE

  }

}
