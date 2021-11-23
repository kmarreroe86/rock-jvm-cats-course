package part1intro

object CatsIntro extends App {

  // Eq => Type class that allow us compare values at compile time forcing to be the same type

  val aComparison: Boolean = 2 == "a string"


  // part 1 - type class import
      import cats.Eq


  // part 2 - import type classes instances for the needed type
      import cats.instances.int._


  // part 3 - use type classes API
  val intEquality = Eq[Int]

  val aTypeSafeComparison = intEquality.eqv(2, 3) // return false
//  val unSafeComparison = intEquality.eqv(2, "3")  code won't compile for different types


  // part 4 - use extension methods(if applicable)
  import cats.syntax.eq._
  val anotherSafeComparison = 2 === 3 // return false
  val negComparison = 2 =!= 3 // return true
//  val invalidComparison = 2 =!= "a string" // doesn't compile
  // extension methods are only visible in the presence of the right type class instance



  // part 5 - extending type classes operations to composite types, e.g. List
  import cats.instances.list._  // we bring Eq[List[Int]] in the scope
  val aListComparison = List(2) === List(3) // return false



  // part 6 - create type class instance for a custom type not automatically supported by Cats
  case class ToyCar(model: String, price: Double)
  implicit val toyCarEq: Eq[ToyCar] = Eq.instance[ToyCar] { (car1, car2) =>
    car1.price == car2.price
  }

  val compareTwoToyCars = ToyCar("Ferrari", 29.99) === ToyCar("Lamborghini", 29.99) // return true


  println(s"compareTwoToyCars = ${compareTwoToyCars}")


}
