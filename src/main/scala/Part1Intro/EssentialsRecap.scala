package Part1Intro

import java.util.concurrent.Executors

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

object EssentialsRecap {
  // SOME scala advanced essentials recap (only some)

  // expressions are evaluated -> reduced to a value
  // instructions vs. expressions
  // side effects do not evaluate to any meaningful value, and yet the program DOES something

  // ---------------------------------------------------------------------  futures
  // FUTURES
  // in scala 2.13 the implementation of the global execution context changes to save some global execution context
  // overhead, and nested futures WON'T RUN IN PARALLEL unless we block the API...
  // starting from 2.13 it is better to define your own execution context:

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8)) // nThreads=8
  val aFuture = Future {
    // some other code evaluated on a nother thread
    42
  }

  // evaluate with onComplete
  aFuture.onComplete({
    case Success(value) => println(s"The value $value")
    case Failure(exception) => println(s"A failure! $exception")
  })

  // or with map, flatMap, filter
  val anotherFuture = aFuture.map(_ + 20)


  // --------------------------------------------------------------------- implicits
  // more complex example
  case class Person(name: String, age: Int)

  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  def listToJson[T](list: List[T])(implicit serializer: JSONSerializer[T]): String =
    list.map(value => serializer.toJson(value)).mkString("[", ",", "]")

  implicit val personSerializer: JSONSerializer[Person] = new JSONSerializer[Person] {
    override def toJson(person: Person) =
      s"""
         |{"name" : "${person.name}"}
         |""".stripMargin
  }

  val personsJson = listToJson(List(Person("Alice", 24), Person("Bob", 32)))
  // implicit argument is used to PROVE THE EXISTENCE of a type

  // implicit methods
  implicit def oneArgCaseClassSerializer[T <: Product]: JSONSerializer[T] = new JSONSerializer[T] {
    override def toJson(value: T) =
      s"""
         |{"${value.productElementName(0)}" : "${value.productElement(0)}"}
         |""".stripMargin.trim
  }

  case class Cat(catName: String)

  val catsToJson = listToJson(List(Cat("Tom"), Cat("Garfield")))

  // in the background: val catsToJson = listToJson(List(Cat("Tom"), Cat("Garfield")))(oneArgCaseClassSerializer[Cat])
  // implicit methods are used to PROVE THE EXISTENCE of a type
  // can be used for implicit conversions (DISCOURAGED)

  //---------------------------- TYPE CLASSES --------------------------------------------------------------------------
  // part 1 - type class definition
  /* trait JSONSerializer[T] {
    def toJson(value: T): String
  }*/

  // part 2 - create implicit type class INSTANCES
  implicit object StringSerializer extends JSONSerializer[String] {
    override def toJson(value: String) = "\"" + value + "\""
  }

  implicit object IntSerializer extends JSONSerializer[Int] {
    override def toJson(value: Int) = value.toString
  }

  implicit object PersonSerializer extends JSONSerializer[Person] {
    override def toJson(value: Person) =
      s"""
         |{ "name" : ${value.name}, "age" : ${value.age} }
         |""".stripMargin.trim
  }

  // part 3 - offer some API
  def convertListToJSON[T](list: List[T])(implicit serializer: JSONSerializer[T]): String =
    list.map(v => serializer.toJson(v)).mkString("[", ",", "]")

  // part 4 - extending the existing types via extension methods
  object JSONSyntax {

    implicit class JSONSerializable[T](value: T)(implicit serializer: JSONSerializer[T]) {
      def toJson: String = serializer.toJson(value)
    }

  }


  def main(args: Array[String]): Unit = {
  }
}
