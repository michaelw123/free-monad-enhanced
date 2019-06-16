

object tradestockwithguard extends App {
  sealed trait Free[F[_], A]  {
    private[this] var ph: A = _
    def flatMap[B](f: A => Free[F, B]): Free[F, B] = this match {
      case Return(a) => f(a)
      case FlatMap(sub, cont, filter) => {
        if (filter(ph))
          FlatMap(sub, cont andThen (_ flatMap f), _ => true)
        else
          Empty(sub,  cont  andThen (_ flatMap f), _ => true )
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
      case FlatMap(sub, cont, filter ) => {
        if (filter(ph))
          FlatMap(sub, cont  andThen (_ foreach f), filter)
        else
          Empty(sub,  cont  andThen (_ foreach f), filter )
      }
    }
    def withFilter(f: A => Boolean): Free[F, A] =  this match {
      case Return(a) => {
        this
      }
      case FlatMap(sub, cont, filter ) => {
        FlatMap(sub, cont, f)
      }
    }



  }
  final case class Return[F[_], A](a: A) extends Free[F, A]
  case class FlatMap[F[_], I, A](sub: F[I], cont: I => Free[F, A],  filter: A => Boolean) extends Free[F, A]
  case class Empty[F[_], I, A](sub: F[I], cont: I => Free[F, A], filter: A => Boolean) extends Free[F, A]


  implicit def lift[A](fa: StockTrade[A]): Free[StockTrade, A] = FlatMap(fa, Return.apply, _ => true)
  implicit def lift[A](fa: AskTell[A]): Free[AskTell, A] = FlatMap(fa, Return.apply, _ => true)


  val s:List[String] = List("aaa", "bbb")




  sealed trait StockTrade[A]
  case class CheckPrice(tick:String) extends StockTrade[String]
  case class Buy(tick: String) extends StockTrade[String]
  case class NoAction(tick: String) extends  StockTrade[String]

  sealed trait AskTell[A]
  case class Ask(message:String) extends AskTell[String]
  case class Tell(message:String) extends AskTell[String]
  case class DoNothing(tick: String) extends  AskTell[String]

  val programs = for {
    price1 <- CheckPrice("GOOG")
    price2 <- CheckPrice("SSS") if price1 == "60"
    price3 <- CheckPrice("TTT")
  } yield ()

  val programs1 = for {
    price1 <- CheckPrice("GOOG")
    price2 <- Buy("SSS") if price1 > "60"
    price3 <- Buy("TTT")if price1 <= "60"

  }{
    println("doing stuff "+price1 +price2 + price3)
  }

  val asktell = for {
    hour <- Ask("What time is it?")
    morning <- Tell("Good Morning")  if (hour <= "12")
    afternoon <- Tell("Good afternoon") if (hour > "12")
  } yield ()

  val asktellExec = new  Executor[AskTell] {
    override def exec[A](fa: AskTell[A]) = fa match {
      case Ask(message) => {
        println("What time is it?")
        "15".asInstanceOf[A]
      }
      case Tell(message) => {
        println(message)
        message.asInstanceOf[A]
      }
      case  DoNothing(message) => message.asInstanceOf[A]
    }
    def execNone[A](fa: AskTell[A]): A = {
      "DoNothing".asInstanceOf[A]
    }
  }
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

      case  NoAction(tick) => tick.asInstanceOf[A]

    }
    def execNone[A](fa: StockTrade[A]): A = {
      "DoNothing".asInstanceOf[A]
    }
  }
  println(programs1)
  // println(programs1)
  runFree(asktell, asktellExec)
  //runFree(programs1, consoleExec)

  def runFree[F[_], A](prg: Free[F, A], executor: Executor[F]): A = {

    prg match {

      case Return(a) => a
      case FlatMap(sub, cont, filter) => {
        runFree(cont(executor.exec(sub)), executor)
      }
      case Empty(sub, cont,filter) => {
        runFree(cont(executor.execNone(sub)), executor)
      }
    }
  }
  sealed trait Executor[F[_]] {
    def exec[A](fa: F[A]): A
    def execNone[A](fa: F[A]): A
  }


}
