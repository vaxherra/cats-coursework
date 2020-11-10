package exercises.Part3DataManipulation

import java.util.concurrent.Executors

import cats.data.Writer
import cats.instances.vector._

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}

object Writers extends App{
  println("--------------------------------- EXERCISE #1 ")
  /*
  TODO EXERCISE #1 =======================================================
  Re-writte a function that prints something with writers
   */

  // stack recursive (given example function)
  def countAndSay(n:Int) : Unit = {
    if(n<=0) println("starting")
    else {
      countAndSay(n-1)
      println(n)
    }
  }
  //println(countAndSay(5))

  // todo rewrite this function in a purely Functional Way, and instead of printing, store them in a writer
  // -------------------------------------------------


  def countAndLog(n:Int) : Writer[Vector[String],Int] = {

    if(n<=0) Writer(Vector("Starting"),n)
    else {
      // countAndLog returns a writer, so we can operate on it
      countAndLog(n-1).flatMap( value => Writer( Vector(s"$n"), n ) )
    }
  }

  // todo optimize to make it TAIL RECURSIVE instead of stack recursive;
  def countAndLogTailRec(n:Int) : Writer[Vector[String],Int] = {
    @tailrec
    def helper(n:Int, writer: Writer[Vector[String],Int]): Writer[Vector[String],Int] = {
      if (n <= 0) writer
      else helper(n - 1,writer.flatMap(value => Writer(Vector(s"$n"), n)))
    }
    helper(n,Writer(Vector("starting"), 0 ))
  }

  println("Count and log stack recursive")
  println(countAndLog(5).run)
  println("Count and log tail recursive")
  println(countAndLogTailRec(5).run)

  /* ==============================================================================================================
  TODO EXERCISE #2 =======================================================
    Rewritte the the below function with writters
   */
  println("--------------------------------- EXERCISE #2 ")

  def naiveSum(n:Int): Int = {
    if(n<=0) 0
    else {
      println(s"Now at step n=$n ") // some side effect
      val lowerSum = naiveSum(n-1)
      println(s"At step ${n-1}, the computed lowerSum =  $lowerSum") // other side effects;
      lowerSum+n
    }
  }

  println("--------- naive sum 5")
  println(naiveSum(2))

  // ---------------------------- SOLUTION ----------------------------------------------

  def naiveSumWithWriters(n:Int): Writer[Vector[String],Int] = {

    def helper(n:Int, acc_writer : Writer[Vector[String],Int]) : Writer[Vector[String],Int] = {
      if(n<=0) {acc_writer}
      else {
        val acc_writer_app = acc_writer.mapWritten(x => x:+s"Now at step n=$n")
        helper(n-1,acc_writer_app).flatMap(value => Writer(Vector(s"At step ${n-1}, the computed lowerSum = ${value}"), value+n))
      } // end of else
    } // end of helper
    helper(n, Writer(Vector(),0))
  }

  // ==== with for comprehensions, without explicitily defining "holder" val's.
  def naiveSumWithWritersFOR(n:Int): Writer[Vector[String],Int] = {
    //stack recursive
    def helper(n:Int, acc_writer : Writer[Vector[String],Int]) : Writer[Vector[String],Int] = {
      if(n<=0) acc_writer
      else for {
        _ <- Writer( Vector(s"Now at step n=$n"),0) // returns a value which we do not use, but keeps the logs,
        // the provided value '0' does not matter (except for the type)
        lowerSum <- helper(n-1, acc_writer ) // returns a Writer, from which we extract the VALUE
        _ <- Writer(Vector(s"At step ${n-1}, the computed lowerSum = ${lowerSum}"), 0)
      } yield lowerSum+n // end of else for
    } // end of helper
    helper(n, Writer(Vector(),0))
  }


  println("--------- naive sum 5, with Writer")
  naiveSumWithWritersFOR(2).written.foreach(println)
  println(naiveSumWithWritersFOR(2).value)

  /// ============================================================================================
  // FUTURES
  println("FUTURES are problematic")
  implicit val ec :ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(4))

  // with our naive example, we'll print out to the console stuff, but we wouldn't know from which parallel thread this
  // log comes from.

  println("NaiveSum without writers with Futures")
  Future(naiveSum(10)).foreach(println) // the logs are going to be interspersed
  Future(naiveSum(10)).foreach(println)

  println("Writer sum of Futures")
  // whereas with writers, it is tractable!
  // we can separate the logs on different threads!
  Future(naiveSumWithWriters(10)).foreach(println)
  Future(naiveSumWithWriters(10)).foreach(println)

}
