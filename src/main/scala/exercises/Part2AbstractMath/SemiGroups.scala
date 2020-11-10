package exercises.Part2AbstractMath

object SemiGroups extends App{
  // ======================================================================== TODO #1 support a new type for a semigroup
  println("----- TODO #1")
  import cats.Semigroup
  import cats.instances.long._
  import cats.instances.double._
  def reduceThings[T](a:List[T])(implicit semigroup: Semigroup[T]): T = a.reduce(semigroup.combine)

  case class Expense(id:Long, amount:Double)

  // ---------------------------------------------- my SOLUTION

  implicit val expenseSemigrop : Semigroup[Expense] = Semigroup.instance[Expense]({
    (expense1, expense2) => Expense(Math.max(expense1.id,expense2.id), expense1.amount+expense2.amount)
  })

  // ---------------------------------------------- my TESTS

  val someExpenses = List (
    Expense(1,99),
    Expense(5546,122.23),
    Expense(55,11.22),
    Expense(2,5353.72)
  )

  println(
    reduceThings(someExpenses)
  )



  // ============================================= TODO #2: define another general API with the combination function |+|
  println("----- TODO #2")
  // Solution :

  // this is how |+| works
  import cats.syntax.semigroup._
  val someCombinedExpenses = Expense(1,22.55) |+| Expense(2,11.22)
  println(someCombinedExpenses)

  def reduceThings2[T](list:List[T])(implicit semigroup: Semigroup[T]): T  = list.reduce(_ |+| _)

  // with the implicit context;
  def reduceThings3[T: Semigroup](list:List[T]): T  = list.reduce(_ |+| _)

  println(reduceThings3(someExpenses))
}
