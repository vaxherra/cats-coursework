package Part1Intro

object CatsIntroduction extends App {

  // TYPE SAFE EQUALITY WITH "EQ"
  // type class "Eq": allows to compare values at COMPILE TIME,
  // the code is not compiled if compared values are OF DIFFERENT TYPES

  val aComparison = 2 == "A string" // a valid expression, always false, however gets compiled

  // ---------------------------------------------------------------------------------
  // an approach similar to the type class approach;
  // PART 1: type class import

  import cats.Eq

  // ---------------------------------------------------------------------------------
  // PART 2: import Type Class instances (TCI) for the types one needs
  import cats.instances.int._ // all the TCI for Int (including Eq)

  // ---------------------------------------------------------------------------------
  // PART 3: Use the Type Class API (explicitly)
  val intEquality = Eq[Int]
  val aTypeSafeComparison = intEquality.eqv(5, 12) // false
  //val aTypeUnsafeComparison =  intEquality.eqv(5,"5") // type mismatch: found String("5") required Int !

  // ---------------------------------------------------------------------------------
  // PART 4: Use extension methods if applicable (to use implicitly)

  import cats.syntax.eq._ // all the extension methods for Eq
  // this uses implicits under the hood; the implicit type class instance in the "val intEquality = Eq[Int]" line 19
  val anotherTypeSafeComparison = 2 === 3 // false
  val notEqualComparison = 2 =!= 5 // true
  // val invalidCompariso1  =        2 =!= "2" // does not compile! (the whole point of type class Eq)

  /*
  extension methods are only visible in the presence of the right Type Class instance;
   */

  // ---------------------------------------------------------------------------------
  // PART 5  - extending the Type Class operations to composite types; e.g. lists
  //val aListComparison = List(2) === List(3) // cannot be converted to an implicit wrapper by default;
  // we need to import instances

  import cats.instances.list._ // importing TCI's
  val aListComparison = List(3, 2) === List(3) // and after importing this works just fine; returns "false"


  // ---------------------------------------------------------------------------------
  // CUSTOM EQ
  // PART 6 - what if our type is not supported by CATS TYPE CLASS INSTANCES

  case class ToyCar(model: String, price: Double) // never use double for money in general applications

  // two toy cars are identical IF THEY HAVE THE SAME PRICE (our definition)
  // CATS provides API to create Type Class Instances (TCIs)

  implicit val toyCarEq: Eq[ToyCar] = Eq.instance[ToyCar]((car1, car2) => car1.price == car2.price)

  val compareTwoToyCars = ToyCar("Ferrari", 25.99) === ToyCar("Yamaha", 25.99) // true


  /* -------------------------------------------------------------------------------------------------------------------
  TAKE HOME

  - most important functionalities are the Type Classess

  to use them:
            1. First import them
              import cats.YourTypeClass

            2. Bring implicit TCI for your supported type in scope
              import cats.instances.yourType._

            3. (more fancy, the implicit way) use extension methods for your TC supports;
              import cats.syntax.youtTypeClass._



    // A Brute import could look like this;
   import cat._
   import cats.implicits._
   */
}
