package Part4TypeClasses

import java.util.concurrent.Executors

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try


object HandlingErrors extends App{
  /*
  Three levels of handling errors:

  1. TRY/CATCH
  2. using Try by encapsulating throwables, JVM errors/esceptions
  3. CATS, pure FP <<<-------------------------------------------------------------------------------------------------
   */

  import cats.Monad
  // E - error type, anything that mimic some/defined Error type
  trait MyMonadError[M[_],E]  extends Monad[M] {
    // 'raiseError' is NOT a fundamental method here on the value type a:A, ...
    // but in reality 'raiseError' is a fundamental method of ApplicativeError (later on in the code discussed)
    def raiseError[A](e:E): M[A] // for now treat it as it is a fundamental method, to get to know the API
  }

  import cats.MonadError
  import cats.instances.either._ // ... implicit MonadError

  type ErrorOr[A] = Either[String, A] // String here is our Error Type
  val monadErrorEither = MonadError[ErrorOr,String]
  /*
  Upon inspecting MonadError we can see that is extends a Monad, so it has pure, flatMap, map, ...
   */
  val success = monadErrorEither.pure(42) // Either[String,Int] => Right(42)
  val failure = monadErrorEither.raiseError[Int]("Something wrong") // provides a potential correct value, and String of failure, as we defined;

  // recovering from an error with handleError
  // the equivalent of "recover" for TRYs
  val handleError : Either[String,Int] = monadErrorEither.handleError(failure){ // where Either[String,Int] == ErrorOr[Int]
    case "SomeError" => 0
    case "SomeOtherError" => -1
    case _ => -999 // unexpected error
  }

  // handleErrorWith,
  // the equivalent of "recoverWith" from TRYs
  val handledErrorWith : ErrorOr[Int] = monadErrorEither.handleErrorWith(failure){
    case "SomeError" => monadErrorEither.pure(0) // Right(0)
    case "Something wrong" => monadErrorEither.pure(-1) // Right(-1)
    case "SomeYetOthererror" => Left("This error I cannot even handle, so go LEFT") // ErrorOr[Int]
    case _ => monadErrorEither.pure(-999) // unexpected error, Right(-999)
  }
  println(handledErrorWith)

  // filter API for changing a value into an error type;
  /// turns a successful value (1st argument list) into a provided error (2nd argument list) if the provided predicate (3rd argument list) is not passed
  val filteredSuccess = monadErrorEither.ensure(monadErrorEither.pure(2))("This is not greater than 10")(x => x>=10 )
  println(filteredSuccess)


  // extension methods for Try and Future
  import cats.instances.try_._ // implicit MonadError[Try], the error type E = Throwable
  val myException = new RuntimeException("Some really bad exception")
  val pureException : Try[Nothing] = MonadError[Try,Throwable].raiseError(myException) // Try[Nothing], and in our case Failure(myException)
  // here, rather than throwing an exception, we store it as information  in Purely Functional way

  import cats.instances.future._  // implicit MonadError[Future]
  implicit val ec : ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val futureMonadError = MonadError[Future,Throwable]
  val pureFutureException = futureMonadError.raiseError(myException) // Future which will complete with Failure(myException)

  /*
  raiseError method does different things depending on the type. So far we had Int, Try, Future;
  However the API remains the same;
   */

  // for applicatives we have ApplicativeError
  import cats.data.Validated
  import cats.ApplicativeError
  import cats.instances.list._ // implicit Semigroup[List] = ApplicativeError[ErrorsOr, List[String]]

  type ErrorsOr[T] = Validated[List[String],T]

  // applicative error also takes TWO type arguments:
  // first - the data wrapper type
  // second the error type
  val applicativeErrorVal = ApplicativeError[ErrorsOr,List[String]] // same API as MonadError, ...
  // so we have pure, raiseError, handleError, handleErrorWith

  import cats.Applicative
  trait MyApplicativeError[M[_],E] extends Applicative[M]  {
    // pure is defined in Applicative
    def raiseError[A](e:E): M[A]
    def handleErrorWith[A](ma:M[A])(func: E=>M[A]): M[A]
    def handleError[A](ma: M[A])(func : E=> A): M[A] = handleErrorWith(ma)(e => pure(func(e)) )
  }

  // and then MyMonadError has this trait signature;
  trait MyMonadError2[M[_],E]  extends MyApplicativeError[M,E] with Monad[M] {
    // the ENSURE is really the fundamental method of MonadError
    def ensure[A](ma: M[A])(error : E)(predicate : A=>Boolean): M[A]
  }

  // the ensure method (line 57) is rather unpleasant to look at, this is why the extensions method (syntax) make it better
  import cats.syntax.applicative._ // pure
  import cats.syntax.applicativeError._ // fundamental methods of applicative raiseError, handleErrorWith, handleError

  val extendedSuccess = 42.pure[ErrorsOr] // requires an implicit ApplicativeError[ErrorsOr], and we have an applicative error "ErrorsOr"
  // reminder:  type ErrorsOr[T] = Validated[List[String],T]

  // to raise an Error
  val extendedError : ErrorsOr[Int] = List("Some bad error").raiseError[ErrorsOr,Int] // Int is the required type, ErrorsOr is the wrapper type
  // this requires an implicit ApplicativeError that requires a List[String] as the error type


  // handleError
  val recoveredError : ErrorsOr[Int] = extendedError.recover{
    case _ => 43
  }


  import cats.syntax.monadError._ // ensure, a fundamental method of MonadError
  // for this we will use an implicit MonadicType, for which we have a MonadError
  // type ErrorOr[A] = Either[String, A] // String here is our Error Type
  // previously defined: val success = monadErrorEither.pure(42) // Either[String,Int] => Right(42)
  val testedSuccess = success.ensure(error = "In case it vailed ")(_>100 )

/*
  THE UPDATED TYPE CLASS HIERARCHY:


 FUNCTOR              SEMIGROUPAL
        \ -- APPLY --/
         /         \
  FlatMap           APPLICATIVE
          \        /           \
             MONAD           APPLICATIVE ERROR
                  \          /
                    MONAD ERROR

II.

    SEMIGROUP
        |
       MONOID

 */



}
