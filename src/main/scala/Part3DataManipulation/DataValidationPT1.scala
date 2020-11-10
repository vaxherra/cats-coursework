package Part3DataManipulation

object DataValidationPT1 extends App{
  /*
  Data validation logic + the errors when the states fails the conditions
   */
  // Validates acts a bit like Either, where the:
  // LEFT SIDE is the undesired type
  // RIGHT SIDE is the desired type

  import cats.data.Validated

  val anInvalidValue : Validated[String,Int] = Validated.invalid("Invalid") // LEFT
  val aValidValue : Validated[String,Int] = Validated.valid(5) // RIGHT

  // "VALIDATED"'s  API
  // testing the condition, and returning valid or invalid
  val aTest : Validated[String,String] = Validated.cond(true, "IF TRUE", "IF FALSE")

  /*
  why don't we use Either?
  Validated is used to combine many "errors" (LEFTs/invalids) into one "giant" error, while using NO MUTATIONS, and
  purely functional programming;
   */

}
