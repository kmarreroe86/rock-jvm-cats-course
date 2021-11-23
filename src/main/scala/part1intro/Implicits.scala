package part1intro

import scala.concurrent.duration.FiniteDuration

object Implicits {

  // implicits classes
  case class Person(name: String) {
    def greet: String = s"Hi, my name is $name!"
  }

  /**
  * -Implicit classes take only one argument
  * -Provide extension methods for a given type enriching it (String this case)
  * */
  implicit class ImpersonableString(name: String) {
    def greet: String = Person(name).greet
  }

  val greeting: String = "Peter".greet  // new ImpersonableString("Peter").greet => Something that receives a string and has a greet method

  // importing implicit conversions in scope
  import scala.concurrent.duration.DurationInt
  val oneSec: FiniteDuration = 1.second


  // implicits arguments and  values
  def increment(x: Int)(implicit amount: Int) = x + amount
  implicit val defaultAmount = 10

  val incremented2 = increment(2) // implicit argument 10 is passed by de compiler

  def multiply(x: Int)(implicit times: Int) = x * 10
  val times2 = multiply(2)  // implicit argument 10 is passed by de compiler


  // More complex example
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  def listToJson[T](list: List[T])(implicit serializer: JSONSerializer[T]): String =
    list.map(value => serializer.toJson(value)).mkString("[", ",", "]")

  implicit val personSerializer = new JSONSerializer[Person] {
    override def toJson(value: Person): String =
      s"""
         |{"name" : "${value.name} }
         |""".stripMargin
  }

  val personJson = listToJson(List(Person("Bob"), Person("Jane")))  // (personSerializer) => supplied by the compiler

  // * implicit argument is used to PROVE THE EXISTENCE of a type


  //  implicit methods
  implicit def oneArgCaseClassSerializer[T <: Product]: JSONSerializer[T] = new JSONSerializer[T] {
    //value.productElementName(0)-> return name of the property at index 0, value.productElement(0) -> return value of the property at index 0
    override def toJson(value: T): String =
      s"""
         |"{${value.productElementName(0)}}" : "{${value.productElement(0)}} "
         |""".stripMargin.trim
  }

  // All case classes implements Product trait
  case class Cat(catName: String)
  val catsToJson = listToJson(List(Cat("Tom"), Cat("Garfield"), Cat("Leopoldo")))
  // in the background: val catsToJson = listToJson(List(Cat("Tom"), Cat("Garfield"), Cat("Leopoldo")))(oneArgCaseClassSerializer[Cat])




  def main(args: Array[String]): Unit = {
    println("main2")
    val catJsonStr = oneArgCaseClassSerializer[Cat].toJson(Cat("Garfield"))
    println(s"catJsonStr = ${catJsonStr}")

    val personJsonStr = oneArgCaseClassSerializer[Person].toJson(Person("Bob"))
    println(s"personJsonStr = ${personJsonStr}")

  }
}
