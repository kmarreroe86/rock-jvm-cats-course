package part3datamanipulation

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Writers {

  import cats.data.Writer
  // 1 - Define writers at the start
  val aWriter: Writer[List[String], Int] = Writer(List("Started something"), 42)

  // 2 - Manipulate them with pure FP
  val anIncreasedWriter = aWriter.map(_ + 1)  // value increases, logs stay the same
  val aLogsWriter = aWriter.mapWritten(_ :+ "found something interesting")  // value stays the same, logs change

  val aWriterWithBoth = aWriter.bimap(_ :+ "found something interesting", _ + 1)  // both value and log change
  val aWriterWithBoth2 = aWriter.mapBoth { (logs, value) =>
    (logs :+ "found something interesting", value + 1)
  } // both value and log change

  // flatMap on Writers
  import cats.instances.vector._  // imports a Semigroup[Vector]
  val writerA = Writer(Vector("Log A1", "Log A2"), 10)
  val writerB = Writer(Vector("Log B1"), 40)
  val compositeWriter = for {
    va <- writerA
    vb <- writerB
  } yield va + vb

  // reset the logs
  import cats.instances.list._  // a Monoid[List[Int]]
  val anEmptyWriter = aWriter.reset // clear the logs, keep the value


  // 3 - Dump either the value or the logs
  val desiredValue = aWriter.value
  val logs = aWriter.written
  val (l, v) = aWriter.run


  // TODO: rewrite a function which prints things with writers
  def countAndSay(n: Int): Unit = {
    if (n <= 0) println("Starting...")
    else {
      countAndSay(n - 1)
      println(n)
    }
  }

  def countAndLog(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0 ) Writer(Vector("Starting..."), n)
    else {
      countAndLog(n - 1).mapBoth { (logs, value) =>
        (logs :+ n.toString, n)
      }
    }
  }

  def countAndLog2(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector("starting!"), 0)
    else countAndLog2(n - 1).flatMap(_ => Writer(Vector(s"$n"), n))
  }

  // Benefit #1: we work with pure FP


  // TODO 2: rewrite this method with writers
  def naiveSum(n: Int): Int = {
    if (n <= 0) 0
    else {
      println(s"Now at $n")
      val lowerSum = naiveSum(n - 1)
      println(s"Computed sum(${n - 1}) = $lowerSum")
      lowerSum + n
    }
  }

  def sumWithLogs(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector(), 0)
    else for {
      _ <- Writer(Vector(s"Now at $n"), n)
      lowerSum <- sumWithLogs(n - 1)
      _ <- Writer(Vector(s"Computed sum(${n - 1}) = $lowerSum"), n)
    } yield lowerSum + n
  }

  // Benefit #2: Writers can keep logs separate on multiple threads

  def main(args: Array[String]): Unit = {
    implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))

//    println(s"compositeWriter.run = ${compositeWriter.run}")
//    countAndSay(10)
//    println("=================")
//    countAndLog(10).written.foreach(println)
//    println("=================")
//    countAndLog2(10).written.foreach(println)
//
//    println("====naiveSum====")
//    naiveSum(10)
//    println("=================")
//    sumWithLogs(10).written.foreach(println)

    println("==== Multiple Threads ====")
    Future(naiveSum(100)).foreach(println)
    Future(naiveSum(100)).foreach(println)

    val sumFuture1 = Future(sumWithLogs(100))
    val sumFuture2 = Future(sumWithLogs(100))
    val logs1 = sumFuture1.map(_.written) // logs from thread 1
    val logs2 = sumFuture2.map(_.written) // logs from thread 2
  }
}

// https://rockthejvm.com/courses/1107955/lectures/23728895
