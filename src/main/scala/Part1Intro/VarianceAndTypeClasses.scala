package Part1Intro

object VarianceAndTypeClasses extends App {
  // THE EFFECTS OF VARIANCE ON TYPE CLASSES
  /*
  How generic variance affects type classes
  - recap of variance
  - how TCs work with variance


   */


  import cats.instances.int._
  import cats.instances.option._
  import cats.syntax.eq._ // Eq[Option[Int]] TC instance;

  // type safe comparison
  val aComparison = Option(2) === Option(3)

  // val anInvalidComparison = Some(2) === None // we do not have access to '===' method by default
  // value === is not a member of Eq[Some[Int]], i.e. it is not found! This is related to VARIANCE
  // the Eq is INVARIANT, and we'd need it to be CONTRAVARIANT

  class Animal

  class Cat extends Animal

  //covariant type: subtyping propagated to the generic type
  class Cage[+T]

  val cage: Cage[Animal] = new Cage[Cat] // this makes sense as Cat <: Animal, so Cage[Cat] <: Cage[Animal]

  // a contravariant type propagates the subtyping backwards to the generic types (ACTION TYPES)
  class Vet[-T]

  val vet: Vet[Cat] = new Vet[Animal] // if Cat <: Animal, then Vet[Cat] >: Vet[Animal]

  /*
  RULE OF THUMB:

 1. if a generic type "HAS A [T]" -> then it usually is COVARIANT
 2. if a generic type "OPERATES/ACTS ON [T]" -> then it usually is CONTRAVARIANT

   */

  //----------------------------------------------------------------------------------------------------
  // THE VARIANCE AFFECTS HOW TYPE CLASSES (TCs) ARE BEING FETCHED

  // contravariant TC
  trait SoundMaker[-T] // "acts"
  implicit object AnimalSoundMaker extends SoundMaker[Animal] //
  def makeSound[T](implicit soundMaker: SoundMaker[T]): Unit = println("<soundzzz>")

  makeSound[Animal] // ok, the compiler finds the implicit AnimalSoundMaker
  makeSound[Cat] // also ok, TC for Animal is also applicable to Cats;

  /* *******************************************
  RULE #1:
      CONTRAVARIANT Type Classes (TCs) can use the superclass instances if nothing is available strictly for that type
   *********************************************/

  implicit object OptionSoundMaker extends SoundMaker[Option[Int]]

  makeSound[Option[Int]]
  makeSound[Some[Int]]

  //----------------------------------------------------------------------------------------------------
  // covariant TC

  // type class definition (I)
  trait AnimalShow[+T] {
    def show: String // irrelevant implementation
  }

  // some Type Class Instances (II)
  implicit object GeneralAnimalShow extends AnimalShow[Animal] {
    override def show: String = "animals, animals everywhere"
  }

  implicit object CatsShow extends AnimalShow[Cat] {
    override def show: String = "cats, cats everywhere"
  }

  // API for users to call (III)
  def organizeShow[T](implicit event: AnimalShow[T]): String = event.show

  println(organizeShow[Cat])
  //println(organizeShow[Animal])
  /*
    the above (commented out) will not compile, as Animal >: Cat,
    and there are two applicable implicits here - GeneralAnimalShow, and CatsShow
  */

  // *******************************************
  // RULE #2 covariant type classes (like AnimalShow) will always "prefer" the more specific type class instances, but may
  // confuse the compiler if more general TCI is also present.
  // *******************************************


  // *******************************************
  // RULE #3: you cannot have both (rule #1 OR rule #2) in generall
  // *******************************************

  // **********************************************************************************************************
  // CATS (THE LIBRARY) uses invariant TYPE classes
  // **********************************************************************************************************

  // the code from the beginning that didn't compile:
  // val anInvalidComparison = Some(2) === None // we do not have access to '===' method by default
  // can be re-written in Cats (a trick) to

  // using the general types & smart constructors;
  Option(2) == Option.empty[Int]


}
