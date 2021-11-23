package part1intro

object TypeClasses {

  case class Person(name: String, age: Int)

  // part 1 - type class definition(trait, abstract class)
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  // part 2 - create implicit type class INSTANCES
  implicit object StringSerializer extends JSONSerializer[String] {
    override def toJson(value: String): String = " \"" + value + "\""
  }

  implicit object IntSerializer extends JSONSerializer[Int] {
    override def toJson(value: Int): String = value.toString
  }

  implicit object PersonSerializer extends JSONSerializer[Person] {
    override def toJson(value: Person): String =
      s"""
         |{"name": ${value.name}, "age": ${value.age} }
         |""".stripMargin.trim
  }

  // part 3 - offer some API
  def convertListToJson[T](value: List[T])(implicit serializer: JSONSerializer[T]): String =
    value.map(el => serializer.toJson(el)).mkString("[", ",", "]")


  // part 4 - extending the existing types via extension methods
  object JSONSyntax {
    implicit class JSONSerializable[T](value: T)(implicit serializer: JSONSerializer[T]) {
      def toJson: String = serializer.toJson(value)
    }
  }


  def main(args: Array[String]): Unit = {
    println(
      convertListToJson(
        List(
          Person("alice", 24), Person("Bob", 42)
        )
      )
    )

    import part1intro.TypeClasses.JSONSyntax._
    println(
      Person("John", 30).toJson // new JSONSerializable(Person("John", 30)).toJson
    )

  }

}
