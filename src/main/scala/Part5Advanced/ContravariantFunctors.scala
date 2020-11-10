package Part5Advanced

object ContravariantFunctors extends App{
  // CONTRAVARIANT FUNCTORS
  // has nothing to do with the generic type variance

  // recap/introduction; A classic type class pattern;
  trait Format[T]{
    def format(value:T): String
  }


/*
  def format[A](value:A)(implicit f : Format[A]) = f.format(value)

  implicit object StringFormat extends Format[String] {
    override def format(value: String): String = "\""+ value + "\""
  }

  implicit object IntFormat extends Format[Int]{
    override def format(value: Int): String = value.toString
  }

  implicit object Boolformat extends Format[Boolean]{
    override def format(value: Boolean): String = if(value) "YES" else "NO"
  }

*/

/*
  println(format(true))
  println(format(42))
  println(format("Makes sense"))
*/

  /*
  THE FREQUENT PROBLEM while creating type class instances:
    Given we have a format[MyType] can we also have Format[Option[MyType]], Format[List[MyType]]?
   */

  implicit def getOptionFormat[T](implicit f: Format[T]): Format[Option[T]] = new Format[Option[T]] {
    override def format(value: Option[T]): String = f.format(value.get)
    // ".get" implementation is bad code (here only for presentation purposes)
  }

  // we can generalize the Option[T] with A into a new function contraMap:
  def contraMap[A,T](func : A=>T)(implicit f: Format[T]): Format[A] = new Format[A]{
    // change of "wrappers"
    override def format(value: A): String = f.format(func(value))
  }

  // so we can translate getOptionFormat into contraMap call:
  implicit def getOptionFormatBetter[T](implicit f:Format[T]) : Format[Option[T]] = contraMap[Option[T],T]( someOption => someOption.get)
  // this is better, but we still use ".get" method


  // hence, we can abstract out contraMap into a Format[T] (which we call BetterFormat[T]) ...
  trait BetterFormat[T]{ self =>
    def format(value:T): String

    def contraMap[A](f: A=>T) : BetterFormat[A] = new BetterFormat[A] {
      override def format(value: A): String = self.format( f(value))
    }
  }

  // ... and amend the getOptionFormatBetter to getOptionFormatGood
  def format[A](value:A)(implicit f : BetterFormat[A]) : String = f.format(value)

  implicit def getOptionFormatGood[T](implicit f:BetterFormat[T]) : BetterFormat[Option[T]] =  f.contraMap[Option[T]]( _.get)

  // improving upon the above, with an implicit Monoid[T]
  import cats.Monoid
  import cats.instances.option._
  implicit def getOptionFormatVeryGood[T](implicit f:BetterFormat[T], m:Monoid[T] ) : BetterFormat[Option[T]] =  {
    f.contraMap[Option[T]]( _.getOrElse(m.empty))
  }

  // example type formatters
  implicit object StringFormat extends BetterFormat[String] {
    override def format(value: String): String = "\""+ value + "\""
  }

  implicit object IntFormat extends BetterFormat[Int]{
    override def format(value: Int): String = value.toString
  }

  implicit object BoolFormat extends BetterFormat[Boolean]{
    override def format(value: Boolean): String = if(value) "YES" else "NO"
  }

  // tests

  println{format(Option(42))}
  println{format(Option("some text"))}

  println{
    format( Option(Option(Option("Some Text #2"))) ) // prints out "Some Text #2"
  }

  /*
  The way the nested options works (example for TWO nested options
  1) StringFormat
  2) <FO> as BetterFormat[Option[String]] = StringFormat.contraMap[Option[String]]( _.get )
  3) <FO2> BetterFormat[Option[Option[String]]] = <FO>.contramap[ Option[Option[String]] ]( _.get )
  4) (and if more nested options are present, expand analogically)

  In reality, it is a CHAIN OF CONTRAMAPS
  <FO2> = StringFormat
  .contramap[Option[String]](_.get)
  .contramap[ Option[Option[String]] ](_.get)

  the order of operations is reversed from the way they are sequenced in the code,
  i.e. first the "inner" Some is evaluated, and then the "outer" one.

  They are evaluated in STACK order, and this is why this is called 'ContraMAP' and NOT A 'MAP',
  i.e. evaluating the transformations in reverse order

  AND FOR THIS REASONS, TYPE CLASSES LIKE 'BETTERFORMAT' ARE CALLED CONTRAVARIANT
   */


  // ====================================================================================================
  import cats.Contravariant
  import cats.Show // similar to format
  import cats.instances.int._ // implicit Show[Int]

  val showInts = Show[Int]
  val showOptions : Show[Option[Int]] = Contravariant[Show].contramap(showInts)(_.getOrElse(0))

  //
  import cats.syntax.contravariant._
  val showOptionsShorter : Show[Option[Int]] = showInts.contramap(_.getOrElse(0))
}
