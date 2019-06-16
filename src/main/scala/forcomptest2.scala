object forcomptest2 extends App {

  def str(x: Int): Option[String] = Some(x.toString)
  def toInt(x: String): Option[Int] = Some(x.toInt)
  def double(x: Int): Option[Double] = Some(x * 2)

  def oldSchool(i: Int) =
    for (x <- str(i);
         y <- toInt(x);
         z <- double(y))
      yield z

  val x = oldSchool(1)
  println(x)

}
