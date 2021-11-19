package part1recap

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object Essentials {

  // generics
  class MyList[A]

  // method notation
  val three: Int = 1 + 2 // infix notation, only for methods that takes 1 parameter
  val anotherThree: Int = 1.+(2)

  // functional programming
  val incrementer: Int => Int = x => x + 1
  val incremented: Int = incrementer(42) // 43

  // higher order functions(map, flatMap, filter...)
  val processedList = List(1, 2, 3).map(incrementer) // List(2,3,4)
  val aLongerList = List(1, 2, 3).flatMap(x => List(x, x + 1)) // List(1,2,2,3,3,4)


  // for-comprehensions
  val checkboard = List(1, 2, 3).flatMap(n => List('a', 'b', 'c').map(c => (n, c))) // cartesian product
  val anotherCheckBoard = for {
    n <- List(1, 2, 3)
    c <- List('a', 'b', 'c')
  } yield (n, c) // equivalent expression

  // Options
  val anOption: Option[Int] = Option(/*something that can be null*/ 3) // Some(3)
  val doubledOption: Option[Int] = anOption.map(_ * 2) //if anOption does not contains value, doubledOption = None,or the doubled value otherwise

  // Try
  val anAttempt = Try(/*something that might throw */ 42) // Success(42)
  val aModifiedAttempt: Try[Int] = anAttempt.map(_ + 10)

  // Pattern Matching
  val anUnknown: Any = 45
  val ordinal = anUnknown match {
    case 1 => "first"
    case 2 => "second"
    case _ => "unknown"
  }

  val optionDescription: String = anOption match {
    case Some(value) => s"the option is not empty $value"
    case None => "the option is empty"
  }

  // Futures (Needs an ExecutionContext, which is a mechanism in charge of scheduling threads)
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val aFuture = Future { // this Future will be running in a different thread
    /* more code here */
    42
  }

  // wait for completion (async, Return a Try[] value). Process using a partial function
  aFuture.onComplete {
    case Success(value) => println(s"The async meaning of live is: $value")
    case Failure(exception) => println(s"Meaning of value failed with: $exception")
  }

  // map a Future
  val anotherFuture: Future[Int] = aFuture.map(_ + 1) // Future(43) when it completes


  // Partial Functions (are based on pattern matching)
  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1 => 42
    case 2 => 43
    case 3 => 100
  }

  val aPartialFunction2: PartialFunction[Int, Int] = { x => x match {
      case 1 => 42
      case 2 => 43
      case 3 => 100
    }
  }

  // Advanced Staff
  // Higher Kinded Types => Generic type that at the same time has a generic parameter type
  trait HigherKindedType[F[_]]
  trait SequenceChecker[F[_]] {
    def isSequential: Boolean
  }

  val listChecker = new SequenceChecker[List] { // Here F is List, which is generic itself
    override def isSequential: Boolean = true
  }




  def main(args: Array[String]): Unit = {

  }

}
