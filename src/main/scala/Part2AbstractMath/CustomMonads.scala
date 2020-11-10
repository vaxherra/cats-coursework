package Part2AbstractMath

import scala.annotation.tailrec

object CustomMonads extends App{
  // CUSTOM MONADS FOR A PARTICULAR TYPE;
  // pure + flatMap functions + iteration methods that need to be stack safe

  import cats.Monad

  //====================================================================================================
  // CUSTOM MONAD OF OPTION

  implicit object OptionMonad extends Monad[Option]{

    override def pure[A](x: A): Option[A] = Option(x)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = {
      // Utilizing Option bulilt-in flatMap
      fa.flatMap(f)
    }

    // additional required function for Monad;
    // Monad has a significance of an iteration, and monads will have some methods starting from a value
    // see OptionMonad.iterate<Tab>...

    @tailrec // important NOT to stack overflow
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match {
      // start with value of Type A, and apply a function F on the value of type A
      // and obtain an instance of option, either A or B
      // IF THE OPTION IS EMPTY! then we need to RUN THE FUNCTION AGAIN
      case None => None
      case Some(Left(v)) => tailRecM(v)(f)// Left is undesirable, and we need to run again
      case Some(Right(v)) => Option(v) // Right is a desirable value
    }

  }




}