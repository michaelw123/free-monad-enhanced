package freemonads

object enhanced extends App {

  sealed trait Free[F[_], A]  {
    private[this] var ph: A = _
    def flatMap[B](f: A => Free[F, B]): Free[F, B] = this match {
      case Return(a) => f(a)
      case FlatMap(sub, cont, filter) => {
        FlatMap(sub, cont andThen (_ flatMap f), filter)
      }
    }
    def  map[B](f: A => B): Free[F, B] = flatMap(a => Return(f(a)))

    def foreach[U](f: A => U) : Free[F, A] = this match {
      case Return(a) => {
        f(a) match {
          case free : Free[F, A] => free
          case _ => this
        }
      }
      case FlatMap(sub, cont, filter ) => {
        FlatMap(sub, cont  andThen (_ foreach f), filter)
      }
    }
    def withFilter(f: A => Boolean): Free[F, A] =  this match {
      case Return(a) => this
      case FlatMap(sub, cont, filter ) => FlatMap(sub, cont, f(ph))
    }



  }
  final case class Return[F[_], A](a: A) extends Free[F, A]
  case class FlatMap[F[_], I, A](sub: F[I], cont: I => Free[F, A],  filter:Boolean) extends Free[F, A]

  implicit def liftF[F[_], A](fa: F[A]): Free[F, A] = FlatMap(fa, Return.apply, true)

  sealed trait AskTell[A]
  case class Ask(message:String) extends AskTell[String]
  case class Tell(message:String) extends AskTell[String]


  val asktell = for {
    hour <- Ask("What time is it?")
    _ <- Tell(s"Good Morning, it is ${hour}am")  if (hour.toInt <= 12)
    _ <- Tell(s"Good afternoon, it is ${hour.toInt -12}pm") if (hour.toInt > 12)
  } yield ()

  val asktell2 = for {
    firstname <- Ask("what is your first name?")
    lastname <- Ask("what is your last name?")
    age <- Ask("what is your age?")
  } {
    println(s"${firstname} ${lastname} ${age}")
  }
  val asktellExec = new  Executor[AskTell] {
    override def exec[A](fa: AskTell[A], filter:Boolean) = fa match {
      case Ask(message) if filter => {
        println(message)
        val hour = scala.io.StdIn.readLine()
        hour.asInstanceOf[A]
      }
      case Tell(message) if filter => {
        println(message)
        message.asInstanceOf[A]
      }
      case  _ => "filtered out".asInstanceOf[A]
    }
  }

  runFree(asktell, asktellExec)
  runFree(asktell2, asktellExec)

  def runFree[F[_], A](prg: Free[F, A], executor: Executor[F]): A = {
    prg match {
      case Return(a) => a
      case FlatMap(sub, cont, filter) => {
        runFree(cont(executor.exec(sub, filter)), executor)
      }
    }
  }
  sealed trait Executor[F[_]] {
    def exec[A](fa: F[A], filter:Boolean): A
  }


}
