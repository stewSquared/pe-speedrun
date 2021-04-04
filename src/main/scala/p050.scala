import algs.primesUntil

object p050 extends App {
  val isPrime = primesUntil(1_000_000)

  val primes = isPrime.toVector

  def prefixes = primes.scanLeft(0)(_ + _)

  val consecutiveUpperLimit = prefixes.takeWhile(_ <= primes.last).length

  def sumOfSlice(i: Int, j: Int): Int = prefixes(j) - prefixes(i)

  def primeSum(consecutive: Int): Option[Int] = Iterator
    .from(0, primes.size - consecutive)
    .map(i => sumOfSlice(i, i+consecutive))
    .takeWhile(_ <= primes.last)
    .find(isPrime)

  def largestPrime(limit: Int): Int =
    primeSum(limit).getOrElse(largestPrime(limit - 1))

  val ans = largestPrime(consecutiveUpperLimit)

  println(ans)
}
