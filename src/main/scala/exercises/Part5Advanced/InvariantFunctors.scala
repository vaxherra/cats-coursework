package exercises.Part5Advanced


object InvariantFunctors extends App{

  /*
  TODO Exercise #1
    Support Option[String] for the "Crypto" trait
      Create an implicit Crypto[Option[String]]
   */

  trait Crypto[A]{ self => // alias to this
    def encrypt(value: A): String
    def decrypt(value : String): A

    def imap[B](backwards: B=>A, forwards: A=>B): Crypto[B] = new Crypto[B]{
      override def encrypt(value: B): String = self.encrypt(backwards(value))
      override def decrypt(value: String): B = forwards(self.decrypt(value))
    }
  }

  def encrypt[A](value :A)(implicit crypto: Crypto[A]): String = crypto.encrypt(value)
  def decrypt[A](value :String)(implicit crypto: Crypto[A]): A = crypto.decrypt(value)

  implicit val casesarCypher :  Crypto[String] = new Crypto[String] {
    override def encrypt(value: String): String = value.map(char => (char+2).toChar)
    override def decrypt(value: String): String = value.map(char => (char-2).toChar)
  }

  implicit val doubleCrypto : Crypto[Double] = casesarCypher.imap(_.toString, _.toDouble )

  //---------------------------------- SOLUTION --------------------------------------------------------
  // A - String
  // B - Option[String]
  implicit val optionStringCrypto : Crypto[Option[String]] =  casesarCypher.imap(_.getOrElse(""), Option(_))

  val someSome = Some("Lets encrypt")
  val someSomeEncrypted = encrypt[Option[String]](someSome)
  val someSomeDecrypted = decrypt[Option[String]](someSomeEncrypted)

  println(someSome)
  println(someSomeEncrypted)
  println(someSomeDecrypted)

  //====================================================================================================
  /*
  TODO Exercise #2
    Generalize the above pattern:
      If you have a Crypto[T] => Crypto[Option[T]] if you have a Monoid[T] in scope
   */


  //----------------------------------- SOLUTION -------------------------------------------------------

  import cats.Monoid
  implicit def cryptoOption[T](implicit c: Crypto[T], m: Monoid[T]): Crypto[Option[T]] = {
    c.imap(_.getOrElse(m.empty), x=>Option(x))
  }

  import cats.instances.double._ // Monoid[Double]
  val encryptedPi = encrypt[Option[Double]](Option(Math.PI))
  val decryptedPi = decrypt[Option[Double]]( encryptedPi )

  println("\nEXERCISE #2")
  println(Option(Math.PI))
  println(encryptedPi)
  println(decryptedPi)


  //====================================================================================================
  /*
  TODO Exercise #3
    Establish a connection between these traits (below), by implementing one in terms of the other

  trait MyInvariant[W[_]] {
    // 'imap' a fundamental method of Invariant
    def imap[A,B](wa: W[A])(forth : A=>B)(back : B=>A) : W[B]
  }

  trait MyContravariant[W[_]]{
    def contramap[A,B](wa:W[A])(back: B=>A): W[B]
  }

  trait MyFunctor[W[_]]{
    def map[A,B](wa:W[A])(forth  : A=>B): W[B]
  }

   */

  //----------------------------------------------------------------------------------------------------
  // SUPERTYPE OF BOTH CONTRAVARIANT FUNCTOR AND "FUNCTOR" (COVARIANT FUNCTOR);

  trait MyInvariant[W[_]] { // INVARIANT FUNCTOR
    // 'imap' a fundamental method of Invariant
    def imap[A,B](wa: W[A])(forth : A=>B)(back : B=>A) : W[B]
  }

  trait MyContravariant[W[_]] extends MyInvariant[W]{ // CONTRAVARIANT FUNCTOR;
    def contramap[A,B](wa:W[A])(back: B=>A): W[B]

    override def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B] = contramap(wa)(back)
  }

  trait MyFunctor[W[_]] extends MyInvariant[W]{ // COVARIANT FUNCTOR;
    def map[A,B](wa:W[A])(forth  : A=>B): W[B]

    override def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B] = map(wa)(forth)
  }

}
