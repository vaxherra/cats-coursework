package Part3DataManipulation


object Writers extends App{
  /*
  Writers are another data manipulation tool in CATS;

  A data type that lets you KEEP TRACK OF USEFUL INFO, while the DATA IS BEING MANIPULATED
   */
  println("\n Basic writer operations: \n")
  import cats.data.Writer

  // takes two type arguments:
  // 1. Logs Type
  // 2. Value Type
  // we can think of it as an Option with two generic types
  val aWriter : Writer[List[String], Int] = Writer(List("Started something","and then this"), 42)

  val anIncreasedWriter = aWriter.map(_+1) // value increases, logs stay the same
  val aLogsWriter = aWriter.mapWritten(_ :+"finally that").map(_+10) // updating logs and values together
  println(aLogsWriter)

  val aWriterWithBoth = aWriter.bimap( _ :+"finally that", _+10 ) // two lambdas at once with bimap
  println(aWriterWithBoth==aLogsWriter) // true

  // this is nice, as logs and values can both influence each other values!
  // like including the old value in the logs, and extract some logs to put into values
  val aWriterWithBoth2 = aWriter.mapBoth( (logs, values) => (logs :+"finally that", values+1) )


  // ===============================================================================================================
  // the point is to define them at the start of an application, and manipulate them in a purely functional way;
  val desiredValue = aWriter.value // extracting values
  val logs = aWriter.written // logs

  //extract both at the same time
  val (logs2,values2) = aWriter.run

  println("\n Composing writers")
  val writerA = Writer(Vector("Log A1","Log A2"),  10)
  val writerB = Writer(Vector("Log B1","Log B2"),  10)

  // Combining writer: you can specify how to combine the VALUES;
  // whereas the logs could be combined by their "natural combination function", and this implies a SEMIGROUP
  import cats.instances.vector._  // imports a SEMIGROUP of vector, where the natural combination is concatenation of vectors;

  val compositeWriter = for {
    va <- writerA
    vb <- writerB
  } yield va+vb // summing up values

  println(compositeWriter.run)

  // ==============================================================================================================
  println("\nResetting the logs, while keeping the values")
  import cats.instances.list._  // Monoid[List[String]]
  // however resetting the log requires implicit instance of the Monoid, as we should fall back to the default empty value;
  println(aWriter.reset.run)


  /// ============================================================================================================
  /*
  Advantages of Writer:
  1. Use pure Functional Programming, without side effects like printing to the console, persistence methods, etc...
  2. Easier to TEST and MAINTAIN
  3. Writers can keep logs on multiple threads separately, and process them as you can see fit;

   */
}
