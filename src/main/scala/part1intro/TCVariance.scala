package part1intro

import cats.instances.int._ // Eq[Int] TC instance
import cats.instances.option._  // construct a Eq[Option[Int]] TC instance
import cats.syntax.eq._

object TCVariance {

  val aComparison = Option(2) === Option(3)

//  val anInvalidComparison = Some(2) === None  // Eq[Some[Int]] not found

  // Variance
  class Animal
  class Cat extends Animal

  // Covariant type: subtyping is propagated to the generic
  class Cage[+T]
  val cage: Cage[Animal] = new Cage[Cat]  // Cat <: Animal, so Cage[Cat] <: Cage[Animal]



  // Contravariant type: subtyping is propagated BACKWARDS to the generic
  class Vet[-T]
  val vet: Vet[Cat] = new Vet[Animal] // Cat <: Animal, then Vet[Animal] <: Vet[Cat]

  /** Rule of thumb:
   * "HAS a T" = covariant,
   * "ACTS on T" = contravariant
   * */

  // Variance affects how TC instances are being fetched

  // Contravariance TC
  trait SoundMaker[-T]
  implicit object AnimalSoundMaker extends SoundMaker[Animal]
  def makeSound[T](implicit soundMaker: SoundMaker[T]): Unit = println("wow")

  makeSound[Animal] // ok - TC instance defined above
  makeSound[Cat]  // ok - TC instance for Animal is also applicable for Cat

  /** Rule 1: Contravariant TCs can use superclass instances if nothing is available strictly for that type e.g. Animal superclass of Cat */

  implicit object OptionSoundMaker extends SoundMaker[Option[Int]]
  makeSound[Option[Int]]
  makeSound[Some[Int]]


  // Covariance TC
  trait AnimalShow[+T] {
    def show: String
  }
  implicit object GeneralAnimalShow extends AnimalShow[Animal] {
    override def show = "animals everywhere"
  }
  implicit object CatsShow extends AnimalShow[Cat] {
    override def show = "so many cats!"
  }
  def organizeShow[T](implicit event: AnimalShow[T]): String = event.show

  /** Rule 2: covariant TCs will always use the more specific TC instance for that type e.g. CatShow */
  // but may confuse the compiler if the general TC is also present

  /** Rule 3: you can't have both benefits*/
  // Cats uses INVARIANT TCs
  Option(2) === Option.empty[Int]


  def main(args: Array[String]): Unit = {
    println(organizeShow[Cat]) // ok - the compiler will inject CatsShow as implicit
    // println(organizeShow[Animal]) // will not compile - ambiguous values
  }
}
