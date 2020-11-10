package exercises.Part4TypeClasses

import cats.Apply

object WeakerMonads extends App{

  /*
  TODO EXERCISE #1
    Does the FlatMap Type Class Extend Apply? Try to implement the "ap" method (fundamental to Apply)
    In the MyFlatMap2 trait.
   */
/*

  trait MyFlatMap2[M[_]] extends Apply[M]{ // WEAKER MONAD
    def flatMap[A,B](container:M[A])(f: A => M[B]): M[B]
    // Apply Type Class (TC) extends Functor, i.e. we have access to the map method
    def ap[A,B](wf: M[A=>B])(wa: M[A]): M[B] = ???
  }
*/

  // --------------------------------------------------------------------------------------------------------------
  trait MyFlatMap2[M[_]] extends Apply[M]{ // WEAKER MONAD
    def flatMap[A,B](container:M[A])(f: A => M[B]): M[B]
    // Apply Type Class (TC) extends Functor, i.e. we have access to the map method
    def ap[A,B](wf: M[A=>B])(wa: M[A]) = {
      flatMap(wa)( a => map(wf)(f => f(a) ) )
      //         |  |        /   \     \/
      //         |  |    M[A=>B] A=>B  B
      //         |  |    \_____   ____/
      //       M[A] A =>      M[B]
      /*
      Analysis:
      - look at the inner map, we unwrap the wrapped function 'wf' with a 'map' into just a function 'f'
      - we want to apply this function A=>B, on some value a:A,
      - so in order to do that, we must also use map/flatMap
      - we choose flatMap, as our required return type is M[B],
      - with this flatMap we're able to extract the value a:A
      - and then use it on extracted function f(a)
      - the trick is to nest a map within a flatMap as shown in the example above

      - although this is a one-liner, it is time consuming to arrive at a solution;
       */
    }
  }

}
