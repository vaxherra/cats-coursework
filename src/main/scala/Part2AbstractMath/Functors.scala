package Part2AbstractMath

import scala.util.Try

object Functors extends App{
  /*
  FUNCTORS are HIGHER KINDED TYPE CLASSES;

   functors provide MAP METHOD (similar to lists, tries, options...) and generalize it
   */

  // myFunctor definition, a simplified one....
  trait myFunctor[F[_]] {
    def map[A,B](init_val : F[A] )(  f: A=>B  ): F[B]
  }

  // however cats already has a nice functor function implemented
  import cats.Functor
  import cats.instances.list._
  import cats.instances.option._
  import cats.instances.try_._ // notice the 'try_' that avoids naming collision
  import cats.instances.int._
  import cats.instances.string._

  // ****************** list functor
  val listFunctor = Functor[List]
  val incrementedNumbers = listFunctor.map(  List(1,2,3)  )( _+10 )
  // functor map takes two argument lists: initial value and the mapping function;
  println(incrementedNumbers)


  // ****************** option functor
  val optionFunctor = Functor[Option]
  val incrementedOption = optionFunctor.map(Option(2))(_+5)
  println(incrementedOption)


  // ****************** try functor
  val tryFunctor = Functor[Try]
  val incrementedTry = tryFunctor.map(Try(42))(_+0)
  println(incrementedTry)


  //====================================================================================================
  /// FUNCTORS BECOME IMPORTANT WHEN WE WANT TO GENERALIZE A TRANSFORMATION;

  def do10Times[F[_]](container : F[Int])(implicit functor: Functor[F]): F[Int] = {
    functor.map(container)(_*10)
  }

  println(
    do10Times(List(1,2,3)),
    do10Times(Option(2)),
    do10Times(Try(5))
  )


  //============== EXTENSION METHODS ===================================================================/
  // extension method for functor is MAP
  // implementing a MAP FOR A CUSTOM DATA TYPES

  import cats.syntax.functor._

  trait Tree[+T]
  case class Leaf[+T](value : T) extends Tree[T]
  case class Branch[+T](value:T , l:Tree[T], r:Tree[T]) extends Tree[T]

  // we can circumvent the CATS TYPE CLASSES invariance, to specify the return types
  object Tree{ // "SMART CONSTRUCTORS"
    def leaf[T](value:T): Leaf[T] = Leaf(value)
    def branch[T](value:T, lt: Tree[T], rt: Tree[T]) : Tree[T] = Branch(value,lt,rt)
  }

  implicit object TreeFunctor extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(someValue) => Leaf(f(someValue))
      // stack recursive (not tail recursive!)
      case Branch(someValue, lt, rt) => Branch(f(someValue), map(lt)(f), map(rt)(f) )
    }
  }
  // an example Tree[Int]
  val anIntTree : Tree[Int] =   Tree.branch(1, Tree.branch(22, Tree.leaf(5), Tree.leaf(17)),
    Tree.branch(30, Tree.leaf(2), Tree.leaf(5)))

  val anIncrementedIntTree = anIntTree.map(_ + 10)

  /*
  FUNCTORS IN REAL LIFE:
    - useful for general APIs
    - any form of data structure TRANSFORMED IN SEQUENCE, specialized data structures for high performance algorithms
    - any "mappable" structures under the same high-level API
   */

}
