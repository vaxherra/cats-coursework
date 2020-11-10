package exercises.Part2AbstractMath

object Monoids extends App{
  // TODO #1
  /*
  Solve the implementation of combineFold[T](list:List[T])(implicit monoid : Monoid[T]) : T = ???
   */
  println("EXERCISE #1 ---------------------------------------------")

  import cats.Monoid
  import cats.instances.int._
  import cats.instances.option._
  import cats.instances.string._
  import cats.syntax.monoid._


  // SOLUTION:
  def combineFold[T](a: List[T])(implicit monoid:Monoid[T]): T = {
    a.foldLeft(monoid.empty)( _ |+| _)
  }

  val aListOfInts = (1 to 1000).toList

  // TESTs:
  println(
    combineFold(aListOfInts),
    combineFold(List(" I ","love ","monoids!"))
  )


  //====================================================================================================
  // TODO #2
  /* Combine a list of phonebooks as Maps[String,Int]

   */

  val phonebooks  = List(
    Map(
      "Alice" ->  235,
      "Robert" -> 236,
      "John" -> 237
    ),
    Map(
      "Rebecca" -> 112,
      "Steve" -> 109,
      "Peter" -> 111
    ),
    Map(
      "Eric" -> 555,
      "Jason" -> 532
    )
  )

    //-------------------------- SOLUTION ----------------------------------------------------------------
  println("EXERCISE #2 ---------------------------------------------")

  // the only solution is to IMPORT THE APPROPRIATE TYPE CLASS INSTANCE for the method from Exercise #1
  import cats.instances.map._
  println(phonebooks)
  println(combineFold(phonebooks))

  //====================================================================================================
  // TODO #3 :
  /*
  A hypothetical situation: an online store where the user has multiple "tabs" open, hence creating multiple
  shopping baskets. After the user hits refresh, they are combined together into one basket with combined list of items.

  Given are the case class and a checkout method signature only:
   */
  println("EXERCISE #3 ---------------------------------------------")

  case class ShoppingCart(items: List[String], total:Double)
  // def checkout(shoppingCarts : List[ShoppingCart]): ShoppingCart = ???


  //-------------------------- SOLUTION ----------------------------------------------------------------
  //defining my own monoid

  implicit val shoppingCartMonoid : Monoid[ShoppingCart] = Monoid.instance[ShoppingCart](
    emptyValue =  ShoppingCart(items=List(), total = 0.0),
    cmb =  (sc1, sc2) => ShoppingCart(sc1.items++sc2.items     ,        sc1.total+sc2.total)
  )


  val sc1: ShoppingCart = ShoppingCart(List("Scala cookbook","Ricky Gervais Show","Art fundamentals"),58.12)
  val sc2: ShoppingCart = ShoppingCart(List("Macbook Pro 16 2020","Ipad Pro 11.9","Apple Pencil 2nd gen"), 4300.52)
  val sc3: ShoppingCart = ShoppingCart(List("Ancestral supplements beef liver", "Art of the samurai book", "PS5"),799.22)


  println(
    combineFold(List(sc1,sc2,sc3)) // returns a single shopping cart
  )



}

