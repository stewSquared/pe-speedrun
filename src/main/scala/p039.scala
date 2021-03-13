object p039 extends App {

  def solutions(p: Int): Seq[(Int, Int, Int)] = {
    val trips = for {
      b <- 1 until p / 2
      a <- 1 to b
      c = p - a - b
    } yield (a, b, c)

    trips.filter { case (a, b, c) => a*a + b*b == c*c}
  }

  val ans = (1 to 1000).maxBy(p => solutions(p).length)

  println(ans)
}
