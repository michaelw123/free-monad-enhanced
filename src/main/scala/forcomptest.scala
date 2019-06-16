import scala.util.Try

//https://medium.com/@olxc/free-monads-explained-pt-1-a5c45fbdac30
//FlatMap(Tell("Greetings!"), (_) =>
//  FlatMap(Ask("What is your name?"), (name) =>
//    Return(Tell(s"Hi $name!"))))


object forcomptest extends App {

  val l = for {
    i <- 0 until 10
    j <- 10 until 20 if (j%2 ==(0))
  } yield ()
  println(l)

  def lift[T1,T2](f: T1 => T2):Try[T1]=>Try[T2] = (triedT: _root_.scala.util.Try[T1]) => triedT.map(f)
  val ll:Array[String] = Array("sss","ddd")
  ll.withFilter( a => true)
  ll.iterator

  val lll: Unit = (intWrapper(0).until(10))
    .foreach((i: Int) =>
      (intWrapper(10).until(20))
        .foreach((j: Int) => println(i.+(j)))
    )
  val llll: Unit = List.apply(1, 2, 3, 4)
    .foreach((a: Int) =>
      List.apply(5, 6, 7, 8)
        .foreach((b: Int) => println(a.*(b)))
    )

  val aaa = for {
    a <- List(1,2,3,4)
    b <- List(2,3,4,5) if  b > 4
  } yield a*b

  val bbb: Unit = List.apply(1, 2, 3, 4)
    .foreach((a: Int) =>
      List.apply(2, 3, 4, 5)
        .withFilter((b: Int) => {
          b.>(4)
        })
        .foreach((b: Int) => println(a.*(b)))
    )

  val fun: String => Boolean = _ == "GOOG"
  println(fun)

  def testfun(v:String, f: String => Boolean) = {
    println (f(v))
  }

  testfun("AAA", fun.apply)


//  val laa = List.apply(3, 4, 5)
//    .withFilter((j: Int) => (j.%(2.==((0)))))
//    .foreach((j: Nothing) => println(j))

  val lbb = List(1,2,3,4).withFilter(_%2 == 0).foreach(println)
}
