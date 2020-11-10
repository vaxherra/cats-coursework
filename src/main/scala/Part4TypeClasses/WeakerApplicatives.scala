package Part4TypeClasses

import cats.{Functor, Semigroupal}

object WeakerApplicatives extends App{
  /*
  APPLY = Weaker form of an APPLICATIVE

  WEAKER APPLICATIVE = FUNCTOR + SEMIGROUPAL + AP METHOD

   */

  trait MyApply[F[_]] extends Functor[F] with Semigroupal[F]{

    def ap [B,T](ff: F[B=>T])(fa:F[B]): F[T]  // fundamental to the APPLY trait

    override def product[A, B](fa: F[A], fb: F[B]): F[(A, B)] = {
      val functionWrapper : F[B => (A,B)] = map(fa)(a =>  /* this needs to be a function */ (b:B) => (a,b) )
      ap(functionWrapper)(fb)
    }

  }

  trait MyApplicative[F[_]] extends MyApply[F]  {
    def pure[A](x:A): F[A] // fundamental method of an applicative is PURE

  }


  /*
  The above is what CATS does behind the scene. It is how they are defined (in terms of method signatures).
   */

  import cats.Apply
  import cats.instances.option._ // fetches implicit Apply[Option]
  val applyOption = Apply[Option] // with the "ap" method
  val funcApp = applyOption.ap( Some( (x:Int) => (x+10).toString+s" is a number after adding $x+10 " ) )(Some(1))
  // takes Some of type A, unwraps it out of some, applies a function on it, possibily returning different type
  // like here Int=>String, and wraps it again with an option: Some(x:String)
  println(funcApp)

  println("\nConverting Tuple of Options, into AN OPTION of tuples")
  import cats.syntax.apply._ // holds extension methods from Apply, like ".tuple"
  val aTupleOfOptions = ( Option(1), Option(2), Option(3) )
  println(aTupleOfOptions)
  // we can transform a tuple of options into an Option of tuples with Apply
  val anOptionOfTuples =  aTupleOfOptions.tupled
  println(anOptionOfTuples)

  println("\n the .mapN method")
  // this takes a function of 3 arguments (as we have 3 options for `aTupleOfOptions`)
  val sumOption = aTupleOfOptions.mapN(_+_+_) // Some(<the sum of 3 options>:Int) == Some(6)
  println(sumOption)

  // the mapN method has the limit UP TO 22 ELEMENTS;
  /*
  there are more exotic method beside mapN, like
  - contramapN
  - imapN
  - tupled (which we used)
  - traverseN
  - apWith
   */


  /*
  This is useful, because it is so convenient. For example the above can be applied to Futures, without the need to
  manually unwrap them with some clunky syntax prone to errors.
   */

  /*


  So far, the hierarchy looks like this:

I.

 FUNCTOR              SEMIGROUPAL
        \ -- APPLY --/
               |
           APPLICATIVE
               |
             MONAD

II.

    SEMIGROUP
        |
       MONOID

   */
}
