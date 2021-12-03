package part2AbstractMath

import scala.util.Try

object Functors {

  /** Functor is a type class that abstracts over type constructors that can be map‘ed over. Examples of such type constructors are List, Option, and Future. */

  val aModifiedList = List(1,2,3).map(_ + 1) // List(2,3,4)
  val aModifiedOption = Option(2).map(_ + 1) // Some(3)
  val aModifiedTry = Try(42).map(_ + 1) // Success(43)

  // simplified definition
  trait MyFunctor[F[_]] { // Higher Kinded type class
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  import cats.Functor
  import cats.instances.list._  // includes Functor[List]

  val listFunctor: Functor[List] = Functor[List]
  val incrementedNumbers = listFunctor.map(aModifiedList)(_ + 1)  // List(3, 4, 5)


  import cats.instances.option._  // includes Functor[Option]
  val optionFunctor = Functor[Option]
  val incrementedOption = optionFunctor.map(Option(2))(_ + 1) // Some(3)


  import cats.instances.try_._
  val anIncrementedTry = Functor[Try].map(Try(1))(_ + 1)  // Success(2)


  // Generalizing an API
  def do10xList(list: List[Int]): List[Int] = list.map(_ * 10)
  def do10xOption(option: Option[Int]): Option[Int] = option.map(_ * 10)
  def do10xTry(attempt: Try[Int]): Try[Int] = attempt.map(_ * 10)

  // generalize
  def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] = functor.map(container)(_ * 10)


  // TODO 1: define your own functor for a binary tree
  // hint: define an object which extends Functor[Tree]
  trait Tree[+T]
  case class Leaf[+T](value: T) extends Tree[T]
  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  object Tree {
    // "smart" constructors
    def leaf[T](value: T): Tree[T] = Leaf(value)
    def branch[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] = Branch(value, left, right)
  }

  implicit object TreeFunctor extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(v) => Leaf(f(v))
      case Branch(v, left, right) => Branch(f(v), map(left)(f), map(right)(f))
    }
  }


  // Extension method - map
  import cats.syntax.functor._
  val tree: Tree[Int] = Tree.branch(40, Tree.branch(5, Tree.leaf(10), Tree.leaf(30)), Tree.leaf(20))
  val incrementedTree = tree.map(_ + 1)


  // TODO 2: write a shorted do10x method using extension methods
  def do10xShort[F[_] : Functor](container: F[Int]): F[Int] = container.map(_ * 10)



  def main(args: Array[String]): Unit = {

    println(s"incrementedNumbers = ${incrementedNumbers}")
    println(s"anIncrementedTry = ${anIncrementedTry}")

    println(s"do10x(List(1,2,3)) = ${do10x(List(1,2,3))}")
    println(s"do10x(Option(2)) = ${do10x(Option(2))}")
    println(s"do10x(Try(3)) = ${do10x(Try(3))}")

    println(s"do10x() = ${do10x(Tree.branch(30, Tree.leaf(10), Tree.leaf(20)))}")
    println(s"do10xShort() = ${do10xShort(Tree.branch(30, Tree.leaf(10), Tree.leaf(20)))}")
  }
}
