object p005 extends App {
  val maxCounts = collection.mutable.Map.empty[Int, Int].withDefaultValue(0)

  // TODO use prime generator for a rewrite

  (2 to 20).foreach { n =>
    algs.factors(n)
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
