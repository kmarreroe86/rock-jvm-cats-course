package part3datamanipulation

// A mechanism by which an expression is reduced to a value
object Evaluation {

  /**
    Cats makes the distinction between
    - evaluating an expression eagerly
    - evaluating lazily and every time you request it
    - evaluating lazily and keeping the value (memoizing)
   */

    import cats.Eval
  val instantEval: Eval[Int] = Eval.now {
    println("Computing now")
    6435
  }

  //*** Will not be evaluated until redoEval.value its called, and its evaluated each time its called
  val redoEval = Eval.always {
    println("Computing again at: " + System.nanoTime())
    4234
  }

  //*** Will not be evaluated, until delayedEval.value its called, will store the value for future calls
  val delayedEval: Eval[Int] = Eval.later {
    println("Computing later at: " + System.nanoTime())
    53278
  }

  val composedEvaluation: Eval[Int] = instantEval.flatMap(value1 => delayedEval.map(value2 => value1 + value2))

  val composedEvaluationFor: Eval[Int] = for {
    value1 <- instantEval
    value2 <- delayedEval
  } yield value1 + value2 // same as above


  // TODO 1: predict the output
  /*
  * "Computing now", "Computing later at:", "Computing again at:", "Computing again at:", result
  *
  * */
  val evalEx1 = for {
    a <- delayedEval
    b <- redoEval
    c <- instantEval
    d <- redoEval
  } yield a + b + c + d

  // "remember a computed value
  val dontRecompute = redoEval.memoize

  val tutorial = Eval
    .always {
      println("Step 1...")
      "put the guitar on your lap"
    }
    .map { step1 =>
      println("Step 2")
      s"$step1 then put your left hand on the neck"
    }.memoize // remember the value up to this point
    .map { steps12 =>
      println("Step 3, more complicated")
      s"$steps12 then with the right hand strike the strings"
    }

  // TODO 2: implement defer such that defer(Eval.now) does NOT run the side effects
  def defer[T](eval: => Eval[T]): Eval[T] =
    Eval.later(()).flatMap(_ => eval)

  // TODO 3: rewrite the method with Evals
  def reverseList[T](list: List[T]): List[T] =
    if (list.isEmpty) list
    else reverseList(list.tail) :+ list.head

  def reverseEval[T](list: List[T]): Eval[List[T]] =
    if (list.isEmpty) Eval.now(list)
    else Eval.defer(reverseEval(list.tail).map(_ :+ list.head))

  def main(args: Array[String]): Unit = {

//    println(s"instantEval.value = ${instantEval.value}")

//    redoEval.value
//    println(s"redoEval.value = ${redoEval.value}")
    
//    delayedEval.value
//    println(s"delayedEval.value = ${delayedEval.value}")

//    println(s"composedEvaluation.value = ${composedEvaluation.value}")
//    println(s"composedEvaluation.value = ${composedEvaluation.value}")

//    println(s"evalEx1.value = ${evalEx1.value}")  // "Computing now", "Computing later at:", "Computing again at:", "Computing again at:", result
//    println(s"evalEx1.value = ${evalEx1.value}")  // "Computing again at:", "Computing again at:", result

//    println(s"tutorial = ${tutorial.value}")
//    println(s"tutorial = ${tutorial.value}")

    /*println(defer(Eval.now {
      println("Now!")
      42
    }).value)*/

    println(reverseEval((1 to 10000).toList).value)

  }

}


// https://rockthejvm.com/courses/1107955/lectures/23728920