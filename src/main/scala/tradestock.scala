import scala.collection.{IterableOnce, IterableOps}

object tradestock extends App {

  sealed trait Free[F[_], A]  {
    def flatMap[B](f: A => Free[F, B]): Free[F, B] = this match {
      case Return(a) => f(a)
      case FlatMap(sub, cont) => FlatMap(sub, cont andThen (_ flatMap f))
    }
     def  map[B](f: A => B): Free[F, B] = flatMap(a => Return(f(a)))



  }
  final case class Return[F[_], A](a: A) extends Free[F, A]
  case class FlatMap[F[_], I, A](sub: F[I], cont: I => Free[F, A]) extends Free[F, A]

  implicit def lift[A](fa: StockTrade[A]): Free[StockTrade, A] = FlatMap(fa, Return.apply)

  val s:List[String] = List("aaa", "bbb")
  //s.withFilter




  sealed trait StockTrade[A]
  case class CheckPrice(tick:String) extends StockTrade[String]
  case class Buy(tick: String, amount:String,  filter: String => Boolean = _ => true) extends StockTrade[(String, String)]


  val programs = for {
    prices <- CheckPrice("GOOG")
     _ <- Buy("GOOG", prices,  checktick)
    prices <- CheckPrice("SSS")
    _ <- Buy("SSS", prices)
  } yield ()



  def checktick(a:String) = a=="GOOG"


  val consoleExec = new Executor[StockTrade] {
    override def exec[A, B](fa: StockTrade[A]) = fa match {
      case CheckPrice(tick) => {
        println("check " + tick)
        if (tick == "GOOG")
          "60".asInstanceOf[B]
        else
          "50".asInstanceOf[B]
      }
      case Buy(tick, amount,  filter )  => if (filter(tick)) println("buy " + tick +" for "+amount)
        (tick, amount).asInstanceOf[B]
    }
  }
  runFree(programs, consoleExec)
  def runFree[F[_], A](prg: Free[F, A], executor: Executor[F]): A = prg match {
      case Return(a) => a
      case FlatMap(sub, cont) => runFree(cont(executor.exec(sub)), executor)
  }
  sealed trait Executor[F[_]] {
    def exec[A, B](fa: F[A]): B
  }


}
