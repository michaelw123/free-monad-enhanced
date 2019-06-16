
//for(x <- c1; y <- c2; z <- c3) yield {...}

//c1.flatMap(x => c2.flatMap(y => c3.map(z => {...})))

//val ns = List(1, 2)
//val os = List (4, 5)

//for (n <- ns; o <- os)  println(n * o)
//ns foreach {n => os foreach {o => println(n * o) }}

object tradestockwithforeach extends App {
  sealed trait Free[F[_], A]  {
    def flatMap[B](f: A => Free[F, B]): Free[F, B] = this match {
      case Return(a) => f(a)
      case FlatMap(sub, cont) => {
        FlatMap(sub, cont andThen (_ flatMap f))
      }
    }
    def  map[B](f: A => B): Free[F, B] = flatMap(a => Return(f(a)))

    def foreach[U](f: A => U) : Free[F, A] = this match {
      case Return(a) => {
        val fa = f(a)
        fa match {
          case free : Free[F, A] => free
          case _ => this
        }
      }
      case FlatMap(sub, cont) => {
        FlatMap(sub, cont  andThen (_ foreach f))

      }
    }
  }
  final case class Return[F[_], A](a: A) extends Free[F, A]
  case class FlatMap[F[_], I, A](sub: F[I], cont: I => Free[F, A]) extends Free[F, A]


   implicit def lift[A](fa: StockTrade[A]): Free[StockTrade, A] = FlatMap(fa, Return.apply)


  val s:List[String] = List("aaa", "bbb")




  sealed trait StockTrade[A]
  case class CheckPrice(tick:String) extends StockTrade[String]
  case class Buy(tick: String) extends StockTrade[String]


  val programs = for {
    price <- lift(CheckPrice("GOOG"))
    price <- lift(CheckPrice("SSS"))
    price3 <- lift(CheckPrice("TTT"))
  } yield ()

  val programs1 = for {
    price1 <- CheckPrice("GOOG")
    price2 <- CheckPrice("SSS")
    price3 <- CheckPrice("TTT")

  }{
    println("doing stuff "+price1 +price2 + price3)
  }

//  val programs1 = for {
//    price1 <- liftForEach(CheckPrice("GOOG"))
//    price2 <- liftForEach(CheckPrice("SSS"))
//  }{
//    println("doing stuff "+price1)
//  }

  def checktick(a:String) = a=="GOOG"


  val consoleExec = new Executor[StockTrade] {
    override def exec[A](fa: StockTrade[A]) = fa match {
      case CheckPrice(tick) => {
        println("check " + tick)
        if (tick == "GOOG")
          "60".asInstanceOf[A]
        else
          "50".asInstanceOf[A]
      }
      case Buy(tick)  => println("buy " + tick)
        tick.asInstanceOf[A]
    }
  }
  println(programs1)
 // println(programs1)
  runFree(programs, consoleExec)
  //runFree(programs1, consoleExec)

  def runFree[F[_], A](prg: Free[F, A], executor: Executor[F]): A = prg match {
    case Return(a) => a
    case FlatMap(sub, cont) => {
      runFree(cont(executor.exec(sub)), executor)
    }
  }
  sealed trait Executor[F[_]] {
    def exec[A](fa: F[A]): A
  }


}
