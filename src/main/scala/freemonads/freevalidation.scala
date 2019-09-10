package freemonads

object freevalidation extends App {
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

  case class NameAge(name:String, age:Int)
  sealed trait Validator[A] {
    def validate(arg:A):A
  }
  case class NameValidator(name:String) extends Validator[String] {
    def validate (name:String) = name
  }
  case class AgeValidator(age:Int) extends Validator[Int] {
    def validate(age: Int) = age
  }
  case class NameAgeValidator(nameage:NameAge) extends Validator[NameAge] {
    def validate(nameage: NameAge) = nameage
  }

  val validators = new  Executor[Validator] {
    override def exec[A](fa: Validator[A]):A = fa match {
      case NameValidator(name) => {
        println("validate name")
        fa.validate(name.asInstanceOf[A])
      }
      case AgeValidator(age) => {
        println("validate age")
        fa.validate(age.asInstanceOf[A])
      }
      case NameAgeValidator(nameage) => {
        println("validate nameage")
        fa.validate(nameage.asInstanceOf[A])
      }
    }
  }

  val validation = for {
    name <- NameValidator("Joe Doe")
    age  <- AgeValidator(18)
    nameage <- NameAgeValidator(NameAge("Michael",55))
  } yield save(nameage)

  val x = validate(validation, validators)

  def save(name:String, age:Int) = println(s"save $name at age $age")
  def save(nameage:NameAge) = println(s"save ${nameage.name} at age ${nameage.age}")

  def validate[F[_], A](prg: Free[F, A], executor: Executor[F]): A = {
    prg match {
      case Return(a) => a
      case FlatMap(sub, cont) => {
        validate(cont(executor.exec(sub)), executor)
      }
    }
  }


  sealed trait Executor[F[_]] {
    def exec[A](fa: F[A]): A
  }
}
