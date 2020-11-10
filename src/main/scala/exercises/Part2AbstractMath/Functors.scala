package exercises.Part2AbstractMath


object Functors extends App{
  // TODO EXERCISE #1
  println("------------------------ Exercise #1")
  /*
  define your own functor for a binary tree
   */

  trait Tree[+T]
  case class Leaf[+T](value : T) extends Tree[T]
  case class Branch[+T](value:T , l:Tree[T], r:Tree[T]) extends Tree[T]


  // ----------------------------- solution
  import cats.Functor

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

  // -------------------- some tests
  def do10Times[F[_]](container : F[Int])(implicit functor: Functor[F]): F[Int] = {
    functor.map(container)(_*10)
  }

  val myCustomTree =  Branch(1, Branch(22, Leaf(5), Leaf(17)), Branch(30, Leaf(2), Leaf(5)))
  val myCustomTree2 =  Tree.branch(1, Tree.branch(22, Tree.leaf(5), Tree.leaf(17)),
                        Tree.branch(30, Tree.leaf(2), Tree.leaf(5)))
  println(
    myCustomTree,
    "\n \t and after using do10Times on myCustomTree \n",
    do10Times[Tree](myCustomTree), // we explicitly specify Tree, so as the compiles does not use Branch instead
    // and the compiler wouldn't find an implicit Functor[Branch]
    // CATS TYPE CLASSES ARE GENERALLY INVARIANT!
    do10Times(myCustomTree2) // without explicit types, using the smart object constructor from lines 19-22
  )

  //====================================================================================================
  // TODO EXERCISE #2
  println("------------------------ Exercise #2")
  /*
  Create a shorted version of do10Times using Functor syntax from scala cats library
   */
  import cats.syntax.functor._
  def do10Times_v2[F[_]: Functor ](container : F[Int]): F[Int] = container.map(_*10)
  // F[_] : Functor means that there is an implicit functor of F,
  // i.e. CONTEXT BOUNDED GENERIC

  println(
    do10Times_v2(myCustomTree2)
  )
}
