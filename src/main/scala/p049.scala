object p049 extends App {

  val primes = algs.primesUntil(10_000).dropWhile(_ < 1000)

  def primePermutations(p: Int): Option[Seq[Int]] = {
    val perms = p.toString.permutations.toSeq.collect {
      case s if primes.contains(s.toInt) => s.toInt
    }

    // TODO: technically, this sliding(3) misses cases like 1487
    perms.sorted.sliding(3).collectFirst {
      case ps@Seq(p1, p2, p3) if p3-p2 == p2-p1 => ps
    }
  }

  // got lucky, fix this

  primes.flatMap(primePermutations).distinct.map(_.mkString) foreach println
}
