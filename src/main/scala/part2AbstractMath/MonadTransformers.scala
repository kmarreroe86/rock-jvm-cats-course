package part2AbstractMath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object MonadTransformers {

  /** Option Transformer */
  import cats.data.OptionT // Option transformer
  import cats.instances.list._ // fetch an implicit Monad[List]
  import cats.instances.future._

  val listNumberOfOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2))) //  a list of Option of Int
  val listOfCharOptions: OptionT[List, Char] = OptionT(List(Option('a'), Option('b'), Option.empty[Char]))

  val listOfTuples: OptionT[List, (Char, Int)] = for {
    char <- listOfCharOptions
    number <- listNumberOfOptions
  } yield (char, number)


  /** Either Transformer */
  import cats.data.EitherT
  val listOfEithers: EitherT[List, String, Int] = EitherT(List( Left("something wrong"), Right(43), Right(42) ))
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val futureOfEither: EitherT[Future, String, Int] = EitherT.right(Future(45))  // wrap over Future(Right(45))

  /*
    TODO exercise
    We have a multi-machine cluster for your business which will receive a traffic surge following a media appearance.
    We measure bandwidth in units.
    We want to allocate TWO of our servers to cope with the traffic spike.
    We know the current capacity for each server and we know we'll hold the traffic if the sum of bandwidths is > 250.
   */
  val bandwidths = Map(
    "server1.rockthejvm.com" -> 50,
    "server2.rockthejvm.com" -> 300,
    "server3.rockthejvm.com" -> 170
  )
  type AsyncResponse[T] = EitherT[Future, String, T] // wrapper over Future[Either[String, T]]

  def getBandwidth(server: String): AsyncResponse[Int] = bandwidths.get(server) match {
    case None => EitherT.left(Future(s"Server $server unreachable"))
    case Some(b) => EitherT.right(Future(b))
  }

  // TODO 1
  // hint: call getBandwidth twice, and combine the results
  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = for {
    band1 <- getBandwidth(s1)
    band2 <- getBandwidth(s2)
  } yield band1 + band2 > 250
  // Future[Either[String, Boolean]]

  // TODO 2
  // hint: call canWithstandSurge + transform
  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] =
    canWithstandSurge(s1, s2).transform {
      case Left(reason) => Left(s"Servers $s1 and $s2 CANNOT cope with the incoming spike: $reason")
      case Right(false) => Left(s"Servers $s1 and $s2 CANNOT cope with the incoming spike: not enough total bandwidth")
      case Right(true) => Right(s"Servers $s1 and $s2 can cope with the incoming spike NO PROBLEM!")
    }
  //  ^^^^^^^^^^^^^^^               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  // Future[Either[String, Boolean]] ---- Future[Either[String, String]]


  def main(args: Array[String]): Unit = {

    println(s"listOfTuples = ${listOfTuples}")
    val resultFuture = generateTrafficSpikeReport("server2.rockthejvm.com", "server3.rockthejvm.com").value
    resultFuture.foreach(println)

  }

}

// https://rockthejvm.com/courses/1107955/lectures/23728894