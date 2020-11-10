package exercises.Part3DataManipulation

object Evaluation extends App{

  /*
  TODO #1:
     think what will the expression return?
   */

  import cats.Eval

  val instant : Eval[Int] = Eval.now[Int]{ // eager evaluation
    println("Evaluating now")
    4}

  val redoEval : Eval[Int] = Eval.always[Int]({
    println("Redoing evaluation")
    42})

  val delayedEval : Eval[Int] = Eval.later[Int]({
    println("Only once")
    55})

  val evalEx1 = for {
    a <- delayedEval
    b <- redoEval
    c <- instant
    d <- redoEval
  } yield a+b+c+d

  println(evalEx1.value)
  println(evalEx1.value)

  // todo: what gets printed before running it?

  /*
  1. First instant evals are evaluated:
    println("Evaluating now")
  2. Then value 'a' print
     println("Only once")
  3. Then the value 'b' print
      println("Redoing evaluation")
  4. Then the value 'c', has no print
  5. Then the value 'd' is redone
      println("Redoing evaluation")
  6. Then the value is returned;
      <INT>
  7. Then  we enter second print, which skips instant and delayed prints, as they are already evaluated
  8. And prints two times redoEval:
      println("Redoing evaluation")
      println("Redoing evaluation")
  9. Then again the value is returned
      <INT>
   */

  // ==============================================================================================================
  /*
  TODO #2
    Implement the method given below 'defer', such that when:
        defer(Eval.now{ print("X"); <something>})
        this should NOT PRINT THE "X"
   */
   // def defer[T](evaluation : => Eval[T] ): Eval[T] = ???

  // ------------------------------------------------------------------------------------------------------------
  def defer[T](evaluation : => Eval[T] ): Eval[T] = {
    // for to the evaluation of LATER

    // creates an empty evaluation of later,
    // and uses its empty value in a flatMap call
    // and transforms the empty Later into the evaluation, wrapping it
    Eval.later( () ).flatMap( emptyValue => evaluation  )
  }
  // the DEFER method is naturally part of the Eval -> Eval.defer

  println("\n\nDEFER TESTS \n")
  val someDeferredEval = defer(Eval.now{
    println("Now?")
    42
  })

  println(someDeferredEval.value)
  println(someDeferredEval.value)

  // the DEFER method is naturally part of the Eval -> Eval.defer
  // ==============================================================================================================
  /*
     TODO #3:
        Rewrite the method with Evals instead of regular values
   */
  // ==============================================================================================================
  def reverseList[T](list:List[T]): List[T] = { // forget about the actual ready implementation -> reverseList(list)
    // example stack recursive call
    if(list.isEmpty) list
    else reverseList(list.tail) :+ list.head
  }

  /// ---------------------- solution
  println("\nEval list reversals: \n")
  def reverseListEval[T](list: List[T]): Eval[List[T]] = {
    // the below implementation is NOT STACK SAFE
    /*
      Eval.later[List[T]]{
      if(list.isEmpty) list
      else reverseList(list.tail) :+ list.head
    }*/

    // this transforms STACK recursion -> TAIL recursion, with the help of Eval.defer / defer
    if(list.isEmpty) Eval.now(list) // returns an eval, eagerly evaluated
      // adding the DEFER method we implemented (or provided by cats Eval), suddenly makes it TAIL RECURSIVE
      // this is how the cats+compiler work in tandem, ...
      // the reverseListEval return an eval, so we can map its values, and perform an operation
    else Eval.defer(reverseListEval(list.tail).map(tail => tail :+ list.head ))
  }

  val aTestList = List(1,2,3,4,5,6)
  println(reverseList(aTestList))
  println(reverseListEval(aTestList).value )

  val aTestListBig = (1 to 10000).toList
  // because the method reverseListEval uses the "defer" method, this works on big list,
  // as it is translated into a TAIL RECURSIVE EXPRESSION -> chain of deferred Evals!
  println(reverseListEval(aTestListBig).value)

}
