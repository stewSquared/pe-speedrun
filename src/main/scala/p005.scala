object p005 extends App {
  def factors(n: Long): List[Int] = {
    def loop(n: Long, k: Int): List[Int] = {
      if (n == 1) Nil
      else if (n % k == 0) k :: loop(n/k, k)
      else loop(n, k+1)
    }
    loop(n, k = 2)
  }

  val maxCounts = collection.mutable.Map.empty[Int, Int].withDefaultValue(0)

  (2 to 20).foreach { n =>
    factors(n)
      .groupBy(identity)
      .mapValues(_.length)
      .foreach { case (f, c) =>
        maxCounts(f) = c max (maxCounts(f))
      }
  }

  val ans = maxCounts
    .map { case (f, c) => math.pow(f,c).toInt }
    .product

  println(ans)
}
