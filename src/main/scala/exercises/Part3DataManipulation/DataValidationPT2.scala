package exercises.Part3DataManipulation

import cats.data.Validated

object DataValidationPT2 extends App{
  /*
  TODO EXERCISE #1
    Implement the `validateForm` method
      the input is a Map of String->String, with name, email and a password
      The rules of the form validation are:
      - the fields must be specified (present KEYS in the MAP)
      - the name must not be an empty string
      - email must be a valid email (simplify to contain "@")
      - password must be strong (simplify to len>=10)
      IF THE FORM was successful return a Valid with Success

  object FormValidation {
    type FormValidation[T] = Validated[List[String],T]

    def validateForm(form: Map[String,String]): FormValidation[String] = ???
  }
*/
  object FormValidation {
    type FormValidation[T] = Validated[List[String],T]

    // --------------------------------- HELPER METHODS
    // HELPER METHODS: map.get returns an option, which we can use to define cats.Validated
    def getValue(form:Map[String,String],fieldName:String) : FormValidation[String] = {
      Validated.fromOption(form.get(fieldName), List(s"The field $fieldName must be specified") )
    }

    def nonBlank(value:String, fieldName:String) : FormValidation[String] = {
      Validated.cond(value.length>0, value, List(s"The field $fieldName must not be blank"))
    }

    def containsAt(value:String): FormValidation[String] = Validated.cond(value.contains("@"), value, List(s"$value is invalid"))

    def checkPassword(pass:String): FormValidation[String] = Validated.cond(pass.length>10, pass, List("Password is too short, needs to be at least 10 characters"))

    // --------------------------------- implementations
    import cats.instances.list._
    import cats.instances.string._

    def validateForm(form: Map[String,String]): FormValidation[String] = {
        getValue(form,"name").andThen(name => nonBlank(name,"name")) // errors relating to the name field
          .combine{ // EMAIL checks
                  getValue(form,"email")
                    .andThen(email => nonBlank(email,"email"))
                    .andThen(email => containsAt(email))
          }
          .combine{ // PASSWORD checks
            getValue(form,"password")
              .andThen(pass => nonBlank(pass,"password"))
              .andThen(pass => checkPassword(pass))
          }
          .map(_ => "Form Passed Successfully")
    }
  }

  //----------------------------------------------------------------------------------------------------
  val someValidMap = Map(
    "name" -> "Robert Kwapich",
    "email" -> "robert.kwapich@gmail.com",
    "password" -> "thisPasswordIsGreaterThan10CharsLong",
  )
  val someInvalidMap = Map(
    "name" -> "",
    "email" -> "myemail",
    "password" -> "short",
  )
  val someInvalidMap2 = Map(
    "email" -> "myemailNotValid",
    "password" -> "PasswordLengthOk",
  )

  println(FormValidation.validateForm(someValidMap))
  println(FormValidation.validateForm(someInvalidMap))
  println(FormValidation.validateForm(someInvalidMap2))


}
