import math.{sqrt, ceil}

object p041 extends App {

  val primes = algs.primesUntil(ceil(sqrt(1_000_000_000)).toInt)

  def isPrime(n: Int): Boolean = {
    primes
      .takeWhile(_ < ceil(sqrt(n)).toInt)
      .forall(p => n % p != 0)
  }

  def isPandigital(n: Int): Boolean = {
    "123456789".startsWith(n.toString.sorted)
  }

  val ans = (99_999_999 to 3 by -1).iterator
    .filter(isPandigital)
    .filter(isPrime)
    .next

  println(ans)
}
