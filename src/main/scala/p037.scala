object p037 extends App {
  val isPrime = algs.primesUntil(1_000_000).toSet

  def isTruncatable(p: Int): Boolean = {
    val s = p.toString
    val truncations = (1 to s.length - 1).flatMap { n =>
      List(s.drop(n), s.dropRight(n))
    }
    truncations.map(_.toInt).forall(isPrime)
  }

  val ans = isPrime.filter(p => p > 10 && isTruncatable(p)).sum

  println(ans)
}
