package Part3DataManipulation

object Evaluation extends App{
  // EVALUATION: a mechanism in which an expression is reduced to a value;
  /*
  CATS makes a distinction between
    TYPE 1. evaluating eagerly an expression - "NOW"
    TYPE 2. evaluating lazily and EVERY TIME you request it - "ALWAYS"
    TYPE 3. evaluating lazily and keeping the value (memoization) - "LATER"

   */

  import cats.Eval

  // TYPE 1
  val instant : Eval[Int] = Eval.now[Int]{ // eager evaluation
    println("Evaluating now")
    4
  }
  //println(instant.value) // without this line, the print is still pefromed


  // TYPE 2: ALWAYS
  val redoEval : Eval[Int] = Eval.always[Int]({
    println("Redoing evaluation")
    42
  })
  // two times prints "Redoing evaluation"
  println(redoEval.value)
  println(redoEval.value)


  // TYPE 3: LATER
  val delayedEval : Eval[Int] = Eval.later[Int]({
    println("Only once")
    55
  })

  println(delayedEval.value)
  println(delayedEval.value)
  println(delayedEval.value)


  /*
  as always, the beauty is that we can apply Functional Programming Principles to these Eval structures
   */

  println("\n\n Composing evalutations:\n")
  val composedEvaluation = instant.flatMap(instantValue => delayedEval.map(delayedValue => instantValue+delayedValue))
  println(composedEvaluation.value) // should not print anything, as instant was already eveluated while initializing
  // and delayed was already assessed once

  println("\n\n Composing with FOR comprehesions:")
  val anotherComposedEvaluation = for {
    val1 <- instant
    val2 <- delayedEval
  } yield val1+val2
  println(anotherComposedEvaluation)
  // ==============================================================================================================

  // from ALWAYS -> LATER
  // redoEval always re-evaluates the expression, but we can use the `.memoize` method to store it, and evaluate
  // lazily only once, i.e. "LATER"
  val dontRecompute = redoEval.memoize

  // chains of computation, that can be calculated and stored!
  val tutorial = Eval
    .always{ println("Step 1"); "It rubs the lotion on its skin."}
    .map{ step1 => println("Step 2"); s"$step1 It does this whenever it is told" }
    .memoize // remember the value at this point
    .map{step12 => println("Step 3"); s"$step12, or else it gets the hose again."} // and this point "is worth repeating"
  // so it occurs after memoization, and is evaluated ALWAYS!
  println(tutorial.value) // step 1,2,3 printed + the final result
  println(tutorial.value) // only step 3 printed + the final result

}