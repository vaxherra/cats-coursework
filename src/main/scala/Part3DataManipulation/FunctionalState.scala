package Part3DataManipulation


object FunctionalState extends App{
  // S: Some State
  // A: the answer/desirable value after a single computation
  type MyState[S,A] =  S=> (S,A)
  // so from a state, we obtain a state of the same type and a value

  // in CATS we have the state already implemented
  import cats.data.State

  val countAndSay : State[Int,String] = State(currentInt => (currentInt+1, s"Counted $currentInt") )

  // we use the the initial value of 10 : Int, that conforms to the type signature
  // we then can use the method .run, that returns an instance of Eval -> i.e we would like to access Eval().value
  // and the final tuple is returned
  val (eleven, counted10) = countAndSay.run( 10).value
  println(eleven,counted10)

  /*
  STATE IS AN ABSTRACTION FOR ITERATIVE COMPUTATION IN PURELY FUNCTIONAL TERMS
   */

  // ----------------------------------------------------------------------

  // EXAMPLE: a bad scala code with VARIABLES (ugh!)
  var a = 10
  a+=1
  val firstComputation = s"added 1 to 10, obtained $a"
  a*=5
  val secondComputation = s"multiplied by 5, obtained $a"


  // rewriting to Pure FP with STATES!

  val firstTransformation = State( (s:Int ) => ( s+1, s"added 1 to ${s}, obtained ${s+1}")  )
  val secondTransformation = State( (s:Int ) => ( s*5, s"multiplied by 5 the number ${s}, obtained ${s*5}")  )

  // and if we want to represent the above iterative program, then we need to compose these TWO states into one,
  // and run to obtain its value

  val compositeTranformation : State[Int, (String,String)] = firstTransformation.flatMap{firstResult =>
    secondTransformation.map{ secondResult => (firstResult,secondResult) }
  }

  println(compositeTranformation.run(10).value) // first adds 1, then multiplies by 5 == 55

  val compositeTransformation2 : State[Int, (String,String)] = for {
    firstRes <- firstTransformation
    secondRes <- secondTransformation
  } yield (firstRes,secondRes)

  println(compositeTransformation2.run(10).value)


  // ----------------------------------------------------------------------------------------------------
  import cats.data.State._
  // FOR DEFINITIONS SEE THE EXERCISES IN exercises/Part3DataManipulation/FunctionalState.scala

  // State companion objects!

  // looks like a sequential program, but we use immutable state + pure functional programming (with map, flatMap...)
  // reducing imperative computations to for comprehensions
  val program : State[Int, (Int,Int,Int)] = for {
    a <- get[Int]
    _ <- set[Int](a+10) // set returns unit
    b <- get[Int]
    _ <- modify[Int](_+42) // modify returns unit
    c <- inspect[Int,Int](_*2)
  } yield (a,b,c)

  println(program.run(0).value)
}
