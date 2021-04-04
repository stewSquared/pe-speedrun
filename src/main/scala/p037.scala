import algs.primesUntil

object p037 extends App {
  val upperLimit = 1_000_000

  val isPrime = primesUntil(upperLimit)
  val primes = isPrime.dropWhile(_ < 10)

  def isTruncatable(p: Int): Boolean = {
    val s = p.toString
    val truncations = (1 to s.length - 1).flatMap { n =>
      List(s.drop(n), s.dropRight(n))
    }
    truncations.map(_.toInt).forall(isPrime)
  }

  val truncatablePrimes = primes.filter(isTruncatable).take(11)

  assert(truncatablePrimes.sizeIs == 11, s"raise upper limit: $upperLimit")

  val ans = truncatablePrimes.sum

  println(ans)
}
