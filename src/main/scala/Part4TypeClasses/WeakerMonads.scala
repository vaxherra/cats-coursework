package Part4TypeClasses

import cats.{Applicative,Apply}
import cats.Monad

object WeakerMonads extends App{
  /*
  WEAKER MONADS have just the flatMap method;

  WEAKER MONADS = APPLY + flatMap method

   */

  trait MyMonad[M[_]] { // M type is itself Generic (higher kinded type)
    def pure[A](value :A): M[A] = ???
    def flatMap[A,B](container:M[A])(f: A => M[B]): M[B] = ???
    def map[A,B](container:M[A])(f: A=>B): M[B] = flatMap(container)(x => pure(f(x)) )
  }

  // but we know that the PURE method is fundamental to APPLICATIVES,
      // also Applicative derives from functor, so we have to override map

  // in addition, the FLATMAP is not FUNDAMENTAL to the MONAD trait that is called FlatMap[M[_]]
  // and in that sense this logic can be-rewritten in the below code:

  trait MyFlatMap[M[_]]{ // WEAKER MONAD
    def flatMap[A,B](container:M[A])(f: A => M[B]): M[B]
  }

  trait MyMonad2[M[_]] extends Applicative[M] with MyFlatMap[M]{ // M type is itself Generic (higher kinded type)
    override def map[A,B](container:M[A])(f: A=>B): M[B] = flatMap(container)(x => pure(f(x)) )
  }
  // ----------------------------------------------------------------------------------------------------------
  /*
  Does the FlatMap trait extend Apply? See EXERCISES for detailed explanations

  Yes, FLATMAP TRAIT EXTENDS APPLY
   */

  import cats.FlatMap
  // upon inspecting the cats source code one can see the signature:
  //            @typeclass trait FlatMap[F[_]] extends Apply[F] {


  // FlaMaps have their own extension methods;
  import cats.syntax.flatMap._  // flatMap extension methods
  import cats.syntax.functor._ // map extension methods

  def getPairsIntChar[M[_] : FlatMap](numbers:M[Int],chars:M[Char]): M[(Int,Char)] = for {
    // given the fact that we have imported the above extension methods we can write
      n <- numbers
      c <- chars
  } yield(n,c)

  // making it more general
  def getPairsGeneral[M[_] : FlatMap, A, B](numbers: M[A], chars: M[B]): M[(A, B)] = for {
    n <- numbers
    c <- chars
  } yield (n, c)


  // ----------------------------------------------------------------------------------------------------------
  /*
  The updated class hierarchy

I.

 FUNCTOR              SEMIGROUPAL
        \ -- APPLY --/
         /         \
  FlatMap           APPLICATIVE
          \        /
             MONAD

II.

    SEMIGROUP
        |
       MONOID
   */

  // ----------------------------------------------------------------------------------------------------------

}
