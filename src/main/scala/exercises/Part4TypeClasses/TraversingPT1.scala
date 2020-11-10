package exercises.Part4TypeClasses

import cats.{Applicative, Monad}

object TraversingPT1 extends App{
  /*
    TODO #1: Implement a 'listTraverse' method
      def listTraverse[F[_] : Monad, A,B](list:List[A])(f: A=> F[B]): F[List[B]] = ???
   */

  // --- solution
  import cats.syntax.applicative._ // pure
  import cats.syntax.flatMap._ // flatMap
  import cats.syntax.functor._ // for

  def listTraverse[F[_] : Monad, A,B](list:List[A])(f: A=> F[B]): F[List[B]] = {

    list.foldLeft( List.empty[B].pure[F] )( (accMonad, listElem) => {
    //list.foldLeft( Monad[F].pure(List.empty[B]) )( (accMonad, listElem) => { // this line also works, without using the syntax.applicative
      val transformedElemWrapped : F[B] = f(listElem)
      for {
        transformedElem <- transformedElemWrapped // B <- F[B]
        elemMonad <- accMonad // List[B] <- F[List[B]]
      } yield elemMonad :+ transformedElem // List[B] :+ B
    }  )
  }

  // now go to the main lecture for generalizations and comments
  import cats.syntax.apply._ // mapN

  def listTraverseBetter[F[_] : Applicative, A,B](list:List[A])(f: A=> F[B]): F[List[B]] = {
    list.foldLeft( List.empty[B].pure[F] )( (accMonad, listElem) => {
      val transformedElemWrapped : F[B] = f(listElem)
      (accMonad, transformedElemWrapped).mapN( _ :+ _ ) // List[B], [B] => some generic Z, but our return type will be List[B],
    })
  }


  // ==============================================================================================================
  /*
  TODO #2:
    Implement the easier version of traverse, without the second parameter list (f A=>F[B]).
    This function is called "SEQUENCE"
        def listSequence[F[_] : Applicative, A ]( list: List[F[A]]): F[List[A]]
    we can use the list traverse from earlier
   */
  import cats.Applicative
  def listSequence[F[_] : Applicative, A ]( list: List[F[A]]): F[List[A]] = {
    listTraverseBetter(list)( x=>x) // or instead of x=>x, the keyword 'identity' does the same trick
  }


  // ==============================================================================================================
  /*
  TODO Exercise #3:
    Predict what would happen when printing the results of the expression 1/2 below.
   */
  import cats.instances.vector._
  val expression1 = listSequence(List(Vector(1,2), Vector(3,4)))
  val expression2 = listSequence(List(Vector(1,2), Vector(3,4),Vector(5,6)))

  /*
  expression1: Vector( List(1,3), List(1,4), List(2,3), List(2,4) )
  expression2: Vector( List(1,3,5), List(1,3,6), List(1,4,5), List(1,4,6), List(2,3,5), List(2,3,6), List(2,4,5), List(2,4,6))

   */
  println(expression1)
  println(expression2)

}
