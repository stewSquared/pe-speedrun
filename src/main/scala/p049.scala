object p049 extends App {

  val primes = algs.primesUntil(10_000).dropWhile(_ < 1000)

  def primePermutations(p: Int): Option[Seq[Int]] = {
    val perms = p.toString.permutations.toSeq.collect {
      case s if primes.contains(s.toInt) => s.toInt
    }

    perms.combinations(3).map(_.sorted).collectFirst {
      case ps@Seq(p1, p2, p3) if p3-p2 == p2-p1 => ps
    }
  }

  primes.flatMap(primePermutations).distinct.map(_.mkString) foreach println
}
