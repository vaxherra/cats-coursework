package exercises.Part4TypeClasses

object CatsApplicative extends App{
  /*
  TODO EXERCISE #1
    Define a method "product" given an implicit Applicative of "W", given the method signature:
    def productWithApplicatives[W[_],A,B](wa: W[A], wb: W[B])(implicit applicative : Applicative[W]) : W[(A,B)] = ???
   */

  import cats.Applicative
  import cats.syntax.applicative._

  // -------------------------------SOLUTION----------------------------------------------------------------------------

  // Well, there does not seem to be a solution to this problem... (Given by exercise answer after 15minutes of research...)
  // but given the "ap" function below, one would be able to pull it off...

  def ap[W[_],B,T]( wf : W[B=>T] )( wb: W[B]): W[T] = ??? // given to you ..., re-try again

  def productWithApplicatives[W[_],A,B](wa: W[A], wb: W[B])(implicit applicative : Applicative[W]) : W[(A,B)] = {
    val functionWrapper : W[B => (A,B)] = applicative.map(wa)(a =>  /* this needs to be a function */ (b:B) => (a,b) )
    // so we create an ad hoc method, that map elements a from wa, into a tuple (a,b) where b:B,
    // however we've not provided b, only a lambda method, for any given b:B

    //ap(functionWrapper)(wb)
    // APPLICATIVE ALREADY HAS THE "AP" METHOD PRESENT
    applicative.ap(functionWrapper)(wb)
  }
  /*
  the BASIC TAKEAWAY is that we can define PRODUCT (a fundamental method of Semigroupal) while in the presence
  on an implicit applicative, but ONLY IF you have access to this "AP" method.

  Yet, the APPLICATIVE already has this applicative method provided, and we could use it in our implementations;

   So the APPLICATIVE CAN BE A SEMIGROUPAL (define a product method in the presence of this ap method, that is guaranteed)
   APPLICATIVES EXTEND SEMIGROUPALS

   */
}
