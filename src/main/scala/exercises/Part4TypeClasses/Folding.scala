package exercises.Part4TypeClasses

import cats.Monoid

object Folding extends App{
  /*
  TODO #1: Warmup exercise
    Implement some methods on list base on the "fold" method (foldLeft or foldRight)
    def map[A,B](list: List[A])(f:A=>B): List[B] = ???
    def flatMap[A,B](list:List[A])( f : A => List[B]): List[B] = ???
    def filter[A](list:List[A])(predicate : A=>Boolean): List[A] = ???
    // such that there is a monoid of A in scope
    def combineAll[A](list:List[A])(implicit  monoid : Monoid[A]): A = ???

   */
  // ----------------------------------------------------------------------------------------------------

  def map[A,B](list: List[A])(f:A=>B): List[B] = {
    // list.foldLeft(List.empty[B] )( (someList, element) =>  someList:+f(element) )
    list.foldRight(List.empty[B])( (element,someList) => f(element) :: someList )
  }

  def flatMap[A,B](list:List[A])( f : A => List[B]): List[B] = {
    list.foldLeft( List.empty[B])( (emptyList,element) => emptyList ++ f(element)    )
    // equivalent
    //list.foldLeft( List.empty[B])( (emptyList,element) => emptyList.foldRight(f(element))(_::_)    )
  }

  def filter[A](list:List[A])(predicate : A=>Boolean): List[A] = {
    list.foldRight(List.empty[A])( (element, someList) => {
      if(predicate(element)) element::someList
      else someList
    })
  }

  // such that there is a monoid of A in scope
  import cats.syntax.semigroup._

  def combineAll[A](list:List[A])(implicit  monoid : Monoid[A]): A = {
    list.foldRight(monoid.empty )( _ |+| _  ) // element and the growing monoid are combined
  }


  import cats.instances.int._
  println{
    combineAll((1 to 10).toList)
  }



}
