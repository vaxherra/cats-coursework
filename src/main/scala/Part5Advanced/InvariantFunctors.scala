package Part5Advanced

object InvariantFunctors extends App{
  // INVARIANT FUNCTORS
  // has nothing to do with the generic type variance


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


  // TESTS:

  val toEncrypt = "Let's encrypt"
  val encryptedValue = encrypt(toEncrypt)
  val decryptedValue = decrypt[String](encryptedValue)

  println(toEncrypt)
  println(encryptedValue)
  println(decryptedValue)

  //====================================================================================================
  /*
  The problem: We'd like to re-use this simple cryptography logic to other types: int, doubles, Option[Int], Option[String]
   */

  implicit val doubleCrypto : Crypto[Double] = casesarCypher.imap(_.toString, _.toDouble )

  println("\nEncrypting math PI")
  val encryptedPI = encrypt(Math.PI) // Double -> String
  val decryptedPI = decrypt[Double](encryptedPI) // String -> Double
  println(Math.PI)
  println( encryptedPI )
  println(decryptedPI)


  //====================================================================================================
  // TODO see exercises "InvariantFunctors.scala"

  import cats.Invariant
  import cats.Show // TC that prints things to the consol
  import cats.instances.string._

  val showStrings = Show[String]
  val showOptionString : Show[Option[String]] = Invariant[Show].imap(showStrings)(Option(_))(_.getOrElse("")) // fa, forwards, backwards

  import cats.syntax.invariant._
  val showOptionString2 : Show[Option[String]] = showStrings.imap(Option(_))(_.getOrElse(""))

  // ====================================================================================================
  /*
  SUMMARY: Final Type Class Hierarchy


                      INVARIANT
                          |      \
                          |   CONTRAVARIANT
                          |
      FOLDABLE         FUNCTOR         SEMIGROUPAL
            \        /        \        /
              TRAVERSE           APPLY
                               /         \
                        FlatMap           APPLICATIVE
                                \        /           \
                                   MONAD           APPLICATIVE ERROR
                                        \          /
                                          MONAD ERROR

  II.

      SEMIGROUP
          |
        MONOID



   */

}