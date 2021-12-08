package part2AbstractMath

import cats.Comparison

object UsingMonads {

  import cats.Monad
  import cats.instances.list._
  val monadList = Monad[List]
  val aSimpleList = monadList.pure(2) // List(2)
  val anExtendedList = monadList.flatMap(aSimpleList)(x => List(x, x + 1))
  // applicable to Option, Try, Future


  // Either is also a monad
  val aManualEither: Either[String, Int] = Right(42)

  // Uses
  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T] = Either[Throwable, T]

  import cats.instances.either._
  val loadingMonad: Monad[LoadingOr] = Monad[LoadingOr] // having undesirable type on the left and generic and desirable type on the right, cats can construct a Monad[Either]
  val anEither = loadingMonad.pure(45)  // LoadingOr[Int] == Right(45)
  val aChangedLoading = loadingMonad.flatMap(anEither)(x => if (x % 2 == 0) Right(x + 1) else Left("Loading meaning of life..."))



  // Imaginary online store
  case class OrderStatus(orderId: Long, status: String)
  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] =
    Right(OrderStatus(orderId, "Ready to ship"))

  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if (orderStatus.orderId > 1000) Left("Not available yet, refreshing data...")
    else Right("Amsterdam, NL")

  val orderId = 456L
  val orderLocation = loadingMonad.flatMap(getOrderStatus(orderId))(orderStatus => trackLocation(orderStatus))

  // using extension methods
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  val orderLocation2: LoadingOr[String] = for {
    orderStatus <- getOrderStatus(orderId)
    location <- trackLocation(orderStatus)
  } yield location

  val orderLocationBetter: LoadingOr[String] = getOrderStatus(orderId).flatMap(orderStatus => trackLocation(orderStatus))


  // TODO: the service layer API of a web app
  case class Connection(host: String, port: String)
  val config = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )
  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]
    def issueRequest(connection: Connection, payload: String): M[String]
  }

  def getResponse[M[_]](service: HttpService[M], payload: String)(implicit monad: Monad[M]): M[String] =
    for {
      conn <- service.getConnection(config)
      response <- service.issueRequest(conn, payload)
    } yield response

  /*
    Requirements:
    - if the host and port are found in the configuration map, then we'll return a M containing a connection with those values
      otherwise the method will fail, according to the logic of the type M
      (for Try it will return a Failure, for Option it will return None, for Future it will be a failed Future, for Either it will return a Left)
    - the issueRequest method returns a M containing the string: "request (payload) has been accepted", if the payload is less than 20 characters
      otherwise the method will fail, according to the logic of the type M
    TODO: provide a real implementation of HttpService using Try, Option, Future, Either
   */
  object HttpServiceOptionImpl extends HttpService[Option] {
    override def getConnection(cfg: Map[String, String]): Option[Connection] =
      for {
        host <- cfg.get("host")
        port <- cfg.get("port")
      } yield Connection(host, port)

    override def issueRequest(connection: Connection, payload: String): Option[String] =
      if (payload.length < 20) Some(s"request ($payload) has been accepted")
      else None
  }

  val responseOption = HttpServiceOptionImpl.getConnection(config).flatMap {conn =>
    HttpServiceOptionImpl.issueRequest(conn, "Hello Http service")
  }

  val responseOptionFor = for {
    conn <- HttpServiceOptionImpl.getConnection(config)
    resp <- HttpServiceOptionImpl.issueRequest(conn, "Hello Http service")
  } yield resp

  // TODO implement another HttpService with LoadingOr or ErrorOr
  object AggressiveHttpService extends HttpService[ErrorOr] {
    override def getConnection(cfg: Map[String, String]): ErrorOr[Connection] =
      if (cfg.contains("host") && cfg.contains("port")) Right(Connection(cfg("host"), cfg("port")))
      else Left(new RuntimeException("Connection could not be established: invalid configuration"))

    override def issueRequest(connection: Connection, payload: String): ErrorOr[String] =
      if (payload.length < 20) Right(s"request ($payload) has been accepted")
      else Left(new RuntimeException("Payload is too large"))
  }

  val errorOrResponse: ErrorOr[String] = for {
    conn <- AggressiveHttpService.getConnection(config)
    response <- AggressiveHttpService.issueRequest(conn, "Hello ErrorOr")
  } yield response


  def main(args: Array[String]): Unit = {

    println(s"responseOption = ${responseOption}")
    println(s"responseOptionFor = ${responseOptionFor}")
    println("==============================================================")

    import cats.instances.option._
    println(s"getResponse(HttpServiceOptionImpl, Hello Option) = ${getResponse(HttpServiceOptionImpl, "Hello Option")}")
    println(s"getResponse(AggressiveHttpService, Hello ErrorOr) = ${getResponse(AggressiveHttpService, "Hello ErrorOr")}")

    println(s"errorOrResponse = ${errorOrResponse}")

  }
}


// https://rockthejvm.com/courses/1107955/lectures/23728903