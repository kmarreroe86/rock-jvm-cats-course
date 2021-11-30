package part2AbstractMath

object Monoids {

  import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.semigroup._  // import |+| extension method

//  |+| is always associative
  val numbers = (1 to 1000).toList

  val sumLeft = numbers.foldLeft(0)(_ |+| _)
  val sumRight = numbers.foldRight(0)(_ |+| _)


  // General API
  //  def combinedFold[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.foldLeft()(_ |+| _) // Semigroup is not enough to provide initial value


  /* MONOIDS */ // Basically same as Semigroup with the capability to provide a initial value with the empty method
  import cats.Monoid
  val intMonoid = Monoid[Int]
  val combineInt = intMonoid.combine(23, 999)  // 1022
  val zero = intMonoid.empty  // 0

  import cats.instances.string._  // brings the implicit Monoid[String]
  val emptyString = Monoid[String].empty  // ""
  val combineString = Monoid[String].combine("I understand", " monoids")


  import cats.instances.option._  // can be able now to construct an implicit Monoid[Option[Int]]
  val emptyOption = Monoid[Option[Int]].empty // None
  val combineOption = Monoid[Option[Int]].combine(Option(2), Option.empty[Int]) // Some(2)
  val combineOption2 = Monoid[Option[Int]].combine(Option(3), Option(6)) // Some(9)

  // Extension Methods for Monoids - |+|
//  import cats.syntax.monoid._   // provides |+|, same as import cats.syntax.semigroup._
  val combineOptionFancy = Option(3) |+| Option(7)


  //TODO 1: implement a combineFold
  def combinedFold[T](list: List[T])(implicit monoid: Monoid[T]): T =
    list.foldLeft(monoid.empty)(_ |+| _)

  val combinedResult = combinedFold(List(1,2,3,4,5))
  val combineResultStr = combinedFold(List("I understand", " monoids"))

  // TODO 2: combine a list of phonebooks as Maps[String, Int]
  // hint: don't construct a monoid - use an import
  val phonebooks = List(
    Map(
      "Alice" -> 235,
      "Bob" -> 647
    ),
    Map(
      "Charlie" -> 372,
      "Daniel" -> 889
    ),
    Map(
      "Tina" -> 123
      )
  )

  import cats.instances.map._ // import implicit Monoid[Map]
  val phonebooksCombined = combinedFold(phonebooks)


  // TODO 3 - shopping cart and online stores with Monoids
  // hint: define your monoid - Monoid.instance
  // hint #2: use combineByFold
  case class ShoppingCart(items: List[String], total: Double)
  implicit val shoppingCartMonoid: Monoid[ShoppingCart] = Monoid.instance(
    ShoppingCart(List(), 0.0),
    {(cart1, cart2) =>
      ShoppingCart(cart1.items ++ cart2.items, cart1.total + cart2.total)
    }
  )

  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart =
    combinedFold(shoppingCarts)

  def main(args: Array[String]): Unit = {
    println(s"sumLeft == sumRight = ${sumLeft == sumRight}")
    println(s"combineInt = ${combineInt}")
    println(s"combineString = ${combineString}")
    println(s"combineOption = ${combineOption}")
    println(s"combinedResult = ${combinedResult}")
    println(s"combineResultStr = ${combineResultStr}")
    println(s"phonebooksCombined = ${phonebooksCombined}")
    println(s"checkout() = ${
      checkout(List(
        ShoppingCart(List("iphone", "shoes"), 799),
        ShoppingCart(List("TV"), 20000),
        ShoppingCart(List(), 0)
      ))}")
  }
}

//https://rockthejvm.com/courses/1107955/lectures/23728900
