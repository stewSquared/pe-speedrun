import algs.primesUntil

object p050 extends App {
  val primes = primesUntil(1_000_000).toVector

  val isPrime = primes.toSet

  val consecutiveUpperLimit = primes
    .scanLeft(0)(_ + _)
    .takeWhile(_ <= primes.last)
    .length

  def primeSum(consecutive: Int): Option[Int] = primes
    .sliding(consecutive).toSeq
    .map(_.sum)
    .takeWhile(_ <= primes.last)
    .find(isPrime)

  def largestPrime(limit: Int): Int =
    primeSum(limit).getOrElse(largestPrime(limit - 1))

  val ans = largestPrime(consecutiveUpperLimit)

  println(ans)
}
