package part2AbstractMath

object Semigroups {

  /** Semigroups COMBINE elements of the same type */

  import cats.Semigroup
  import cats.instances.int._

  val naturalIntSemigroup: Semigroup[Int] = Semigroup[Int]
  val intCombination: Int = naturalIntSemigroup.combine(2, 46) // addition

  import cats.instances.string._

  val naturalStringSemigroup: Semigroup[String] = Semigroup[String]
  val stringCombination = naturalStringSemigroup.combine("Scala ", "Rocks")


  // Specific Api
  def reduceInts(list: List[Int]): Int = list.reduce(naturalIntSemigroup.combine)

  def reduceStrings(list: List[String]): String = list.reduce(naturalStringSemigroup.combine)


  // General API
  def reduceThings[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.reduce(semigroup.combine)

  // TODO: Exercise: support a new type(hint: use the same pattern we used with Eq)
  case class Expense(id: Long, amount: Double)

  implicit val expenseSemigroup: Semigroup[Expense] = Semigroup.instance[Expense] { (expense1, expense2) =>
    val newId = Math.max(expense1.id, expense2.id)
    Expense(newId, expense1.amount + expense2.amount)
  }


  // Extension methods from Semigroup -> |+|

  import cats.syntax.semigroup._

  val anIntSum: Int = 2 |+| 3 // requires the presence of an implicit Semigroup[Int], given in: import cats.instances.int._
  val aStringConcatenation: String = "we love " |+| "semigroups"
  val expenseResume: Expense = Expense(1L, 5.5) |+| Expense(2, 1.5)

  //TODO: implement reduceThings2 with the |+|
  def reduceThings2[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.reduce(_ |+| _)
  def reduceThings3[T : Semigroup](list: List[T]): T = list.reduce(_ |+| _) // Using type context, compiler will has access to an implicit semigroup of T

  def main(args: Array[String]): Unit = {

    println(s"intCombination = ${intCombination}")
    println(s"stringCombination = ${stringCombination}")

    val numbers = (1 to 10).toList
    println(s"reduceInts(numbers) = ${reduceInts(numbers)}")

    val numbersStr = (1 to 10).map(s => s.toString + " ").toList
    println(s"reduceStrings(numbersStr) = ${reduceStrings(numbersStr)}")

    println("Using General API:")
    println(s"reduceThings(numbers) = ${reduceThings(numbers)}") // compile injects the implicit Semigroup[Int]
    println(s"reduceThings(numbersStr) = ${reduceThings(numbersStr)}") // compile injects the implicit Semigroup[String]


    import cats.instances.option._
    // compiler will produce an implicit Semigroup[Option[Int]] - combine will produce another option with the summed elements
    // compiler will produce an implicit Semigroup[Option[String]] - combine will produce another option with the concatenated elements

    val numberOptions: List[Option[Int]] = numbers.map(n => Option(n))
    println(s"reduceThings(numberOptions) = ${reduceThings(numberOptions)}") // an Option[Int] containing the sum of all the numbers Some(55)
    println(s"reduceThings3(numberOptions) = ${reduceThings3(numberOptions)}")

    val stringOptions: List[Option[String]] = List("Hello ", "World ", "Scala ", "Rocks").map(s => Option(s))
    println(s"reduceThings(stringOptions) = ${reduceThings(stringOptions)}")
    println(s"reduceThings2(stringOptions) = ${reduceThings2(stringOptions)}")

    println("Test support custom type Expense:")
    val expenses: List[Expense] = List(
      Expense(1L, 12.5), Expense(3L, 5.5),
      Expense(2L, 10.5), Expense(4L, 1.5)
    )
    println(s"reduceThings(expenses) = ${reduceThings(expenses)}")
    println(s"reduceThings3(expenses) = ${reduceThings3(expenses)}")

  }

}

// https://rockthejvm.com/courses/1107955/lectures/23728891