import algs.primesUntil

@main def p060(): Unit = {
  val primes = algs.primesUntil(10000).toSeq

  val concatsWith = primes.map { p =>
    p -> primes.dropWhile(_ <= p)
      .filter(q => algs.isPrime(s"$p$q".toInt))
      .filter(q => algs.isPrime(s"$q$p".toInt))
      .toSet
  }.toMap

  def grow(ps: Set[Int]): Seq[Set[Int]] = {
    val common = ps.toSeq.map(concatsWith).reduce(_ intersect _)
    common.toSeq.map(ps + _)
  }

  val seeds = primes.map(Set(_))

  def sets = Iterator.iterate(seeds)(_.flatMap(grow))

  val ans = sets.drop(4).next.minBy(_.sum)

  println(ans)
  println(ans.sum)
}
