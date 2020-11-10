package exercises.Part2AbstractMath

import scala.annotation.tailrec

object CustomMonads extends App{
  // TODO #1
  /*
  define a monad for the identity type
   */

  type Identity[T] = T
  val aNumber : Identity[Int] = 42


  // ------------------------------- solution
  import cats.Monad

  implicit object IdentityMonad extends Monad[Identity]{
    override def pure[A](x: A): Identity[A] = x
    override def flatMap[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] = f( fa )

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] = f(a) match {
      case Left(v)  => tailRecM(v)(f)
      case Right(v) => pure(v)
    }
  }
  //====================================================================================================
  // TODO #2 MONAD FOR A BINARY TREE example

  // given the data structure;
  sealed trait Tree[+A]
  final case class Leaf[+A](value:A) extends Tree[A]
  final case class Branch[+A](left: Tree[A], right:Tree[A]) extends Tree[A]

  // define a monad for this tree


  // ----------------------------------- solution

  implicit object TreeMonad extends Monad[Tree]{
    override def pure[A](x: A): Tree[A] = Leaf(x)

    // stack-recursive implementation!
    override def flatMap[A, B](tree: Tree[A])(f: A => Tree[B]): Tree[B] = tree match {
      case Leaf(v) => f(v)
      case Branch(l,r) => Branch(  flatMap(l)(f), flatMap(r)(f)     )
    }

    // !!! this is not @tailrec, but stackRec
    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      // an auxiliary function (stack recursive)
      def stackRecursive(t: Tree[Either[A,B]] ): Tree[B]  = t match {
          // left is an undesirable value
        case Leaf(Left(v)) => stackRecursive( f(v) )
        case Leaf( Right(v) ) => Leaf(v) // desirable case _: exercises.Part2AbstractMath.CustomMonads.Leaf[_] =>
        case Branch(left,right) => Branch( stackRecursive(left), stackRecursive(right) )
      }

      // stackRecursive( f(a) ) // less effective call (stack-recursive)

      @tailrec // a better implementation with tail-recursion, however more complicated;
     def tailRec(todo: List[Tree[Either[A, B]]], expanded: Set[Tree[Either[A, B]]], done: List[Tree[B]]): Tree[B] =
       if (todo.isEmpty) done.head
       else todo.head match {
         case Leaf(Left(v)) => tailRec(f(v) :: todo.tail, expanded, done)
         case Leaf(Right(b)) => tailRec(todo.tail, expanded, Leaf(b) :: done)
         case node @ Branch(left, right) =>
           if (!expanded.contains(node)) {
             tailRec(right :: left :: todo, expanded + node, done)
           } else {
             val newLeft = done.head
             val newRight = done.tail.head
             val newBranch = Branch(newLeft, newRight)
             tailRec(todo.tail, expanded, newBranch :: done.drop(2))
           }
       }

      tailRec(List(f(a)), Set(), List())


    }
  } // end of tailRecM


  // testing the tree monad


  val anExampleTree =  Leaf(2)
  val anExampleTree2 = Branch(
    // left
    Branch( Leaf(12), Leaf(25)),
    //right
    Branch( Leaf(12), Branch( Leaf(12), Leaf(52)  ) )
  )

  println(s"An example tree $anExampleTree")

  val aChangedExampleTree = TreeMonad.flatMap(anExampleTree)( v => Branch(Leaf(v-1), Branch(Leaf(v),Leaf(v+1))  ))

  println(s"Changed by monad $aChangedExampleTree")
}
