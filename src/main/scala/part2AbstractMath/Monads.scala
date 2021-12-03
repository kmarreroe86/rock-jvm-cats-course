package part2AbstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Monads {

  /* List */
  // TODO 1.1: how do you create all combinations of (number, char)?
  val numbersList = List(1, 2, 3)
  val charsList = List('a', 'b', 'c')
  val combinations: List[(Int, Char)] = for {
    n <- numbersList
    c <- charsList
  } yield (n, c)

  val combinations2: List[(Int, Char)] = numbersList.flatMap(n => charsList.map(c => (n, c)))


  /* Options */
  val numberOption = Option(2)
  val charOption = Option('d')
  // TODO 1.2: how do you create the combination of (number, char)?
  val tupleOption: Option[(Int, Char)] = numberOption.flatMap(nOp => charOption.map(cOp => (nOp, cOp)))
  val tupleOption2 = for {
    n <- numberOption
    c <- charOption
  } yield (n, c)

  /* Futures*/
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val numberFuture: Future[Int] = Future(42)
  val charFuture: Future[Char] = Future('Z')
  // TODO 1.3: how do you create the combination of (number, char)?
  val combinationsFuture: Future[(Int, Char)] = for {
    n <- numberFuture
    c <- charFuture
  } yield (n, c)

  val combinationsFuture2: Future[(Int, Char)] = numberFuture.flatMap(n => charFuture.map(c => (n, c)))


  /*
  * Pattern
  * -wrapping a value into a M value
  * -the flatMap mechanism transform the value wrapped in a new value also wrapped by a M
  *
  * MONADS
  * */
  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A]   // pure constructs a M[A] of a value A Eg. List(1) => wraps an Int into a List
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
  }


  import cats.Monad
  import cats.instances.option._  // implicit Monad[Option]
  val optionMonad = Monad[Option]
  val anOption: Option[Int] = optionMonad.pure(2) // Option(2) == Some(2)
  val aTransformedOption = optionMonad.flatMap(anOption)(x => if (x % 3 == 0) Some(x + 1) else None)  // None


  import cats.instances.list._
  val listMonad = Monad[List]
  val aList = listMonad.pure(3) // List(3)
  val aTransformedList = listMonad.flatMap(aList)(x => List(x, x))  // List(3, 4)


  // TODO 2: use a Monad[Future]
  import cats.instances.future._
  val futureMonad = Monad[Future] // requires an implicit ExecutionContext
  val aFuture: Future[Int] = futureMonad.pure(1)
  val aTransformedFuture = futureMonad.flatMap(aFuture)(x => Future(x + 1)) // Future(2)


  // specialized API
  def getPairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] = numbers.flatMap(n => chars.map(c => (n, c)))
  def getPairsOption(number: Option[Int], char: Option[Char]): Option[(Int, Char)] = number.flatMap(n => char.map(c => (n, c)))
  def getPairsFuture(number: Future[Int], char: Future[Char]): Future[(Int, Char)] = number.flatMap(n => char.map(c => (n, c)))


  // generalize
  def getPairs[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] =
    monad.flatMap(ma)(a => monad.map(mb)(b => (a, b)))




  def main(args: Array[String]): Unit = {

    println(s"combinations = ${combinations}")
    println(s"tupleOption = ${tupleOption}")

    println(s"combinationsFuture2 = ${combinationsFuture2}")

    println(s"getPairs(numbersList, charsList) = ${getPairs(numbersList, charsList)}")
    println(s"getPairs(numbersList, charsList) = ${getPairs(numbersList, charsList)}")
    println(s"getPairs(numberFuture, charFuture).foreach(println) = ${getPairs(numberFuture, charFuture).foreach(println)}")

  }
}

// https://rockthejvm.com/courses/1107955/lectures/23728902
