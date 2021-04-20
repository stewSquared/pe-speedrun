import fractions.over

@main def p070(): Unit = {
  val limit = 10_000_000

  val primes = algs.primesUntil(10_000).toVector

  val candidates = for {
    p <- primes
    q <- primes.dropWhile(_ <= p).takeWhile(_ <= limit / p)
    n = p * q
    phi = (p - 1) * (q - 1)
    if n.toString.sorted == phi.toString.sorted
  } yield n over phi

  val ans = candidates.min.numerator

  println(ans)
}
