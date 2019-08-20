package freemonads

object basic extends App {
  sealed trait Free[F[_], A]  {
    def flatMap[B](f: A => Free[F, B]): Free[F, B] = this match {
      case Return(a) => f(a)
      case FlatMap(sub, cont) => {
        FlatMap(sub, cont andThen (_ flatMap f))
      }
    }
    def  map[B](f: A => B): Free[F, B] = flatMap(a => Return(f(a)))
  }
  final case class Return[F[_], A](a: A) extends Free[F, A]
  case class FlatMap[F[_], I, A](sub: F[I], cont: I => Free[F, A]) extends Free[F, A]

  implicit def liftF[F[_], A](fa: F[A]): Free[F, A] = FlatMap(fa, Return.apply)

  sealed trait AskTell[A]
  case class Ask(message:String) extends AskTell[String]
  case class Tell(message:String) extends AskTell[String]



  val asktell = for {
    name <- Ask("What is your name?")
    _    <- Tell(s"Hello ${name}!")
  } yield ()

  val asktellExec = new  Executor[AskTell] {
    override def exec[A](fa: AskTell[A]):A = fa match {
      case Ask(message) => {
        println(message)
        val name = scala.io.StdIn.readLine()
        name.asInstanceOf[A]
      }
      case Tell(message) => {
        println(message)
        message.asInstanceOf[A]
      }
    }
  }

  runFree(asktell, asktellExec)


  def runFree[F[_], A](prg: Free[F, A], executor: Executor[F]): A = {
    prg match {
      case Return(a) => a
      case FlatMap(sub, cont) => {
        runFree(cont(executor.exec(sub)), executor)
      }
    }
  }
  sealed trait Executor[F[_]] {
    def exec[A](fa: F[A]): A
  }


}
