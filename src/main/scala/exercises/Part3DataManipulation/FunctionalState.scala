package exercises.Part3DataManipulation

object FunctionalState extends App{
  /*
  TODO Exercise #1
     An online STORE when we sell some products. A very naive example:
   */

  import cats.data.State

  case class ShoppingCart(items: List[String], total:Double)

  // represents a transformation of Shopping Cart to another shopping cart, while keeping the
  def addToCart(item:String, price:Double): State[ShoppingCart,String] = State { cart =>
    ( ShoppingCart(item :: cart.items, price + cart.total ), s" Added $item of price $price to your shopping cart," +
      s" and the new total in the cart is ${price+cart.total}. "  )
  }


  val robertsCart : State[ShoppingCart,String] = for {
     purchase1log <- addToCart("Cyberpunk 2077", 59.99) // addToCart returns a state, which we can use in for expression
    purchase2log <- addToCart("PlayStation5",  499.99)
    purchase3log <- addToCart("PS5 VR",399.99)
  } yield purchase1log+purchase2log+purchase3log

  println(  robertsCart.run(  ShoppingCart(List(),0)  ).value        )


  //====================================================================================================
  /*
  TODO Exercise #2:
    #subtask 1:
        Implement the inspect function of the below signature.
        The function when run does not change the initial state, but it will output the value f(a):B
    # subtask 2:
        Implement the get method of signature given by get_signature.
        Method makes no changes, and returns the exact value of that state when run
    # subtask 3: <...> set method with set_signature
        This method won't run anything, returning a unit, and it will change the state to provided argument
    # subtask 4: <...> modify, with modify signature
        Returns a state data structure when run, it will return Unit and then set the state to f(state)
   */

  def inspect_signature[A,B](f : A=>B): State[A,B] = ???
  def get_signature[A]: State[A,A] = ???
  def set_signature[A](value:A) : State[A, Unit] = ???
  def modify_signature[A](f : A=>A): State[A,Unit] = ???
  //--------------------THE SOLUTION-------------------------------------------------------------------

  def inspect[A,B](f : A=>B): State[A,B] =  State( (a:A) => (a,f(a)))
  def get[A]: State[A,A]                 =  State( (a:A) => (a,a))
  def set[A](value:A) : State[A, Unit]   =  State( (a:A) => (value,()))
  def modify[A](f : A=>A): State[A,Unit] =  State( (a:A) => (f(a),()))

  // all these methods above are already implemented in the companion object of state
  import cats.data.State._
}
