// import algs.erastothenes
import algs.primesUntil

object p060 extends App {

  val primes = primesUntil(10_000_000)

  val isPrime = primes.toSet

  val upperLimit = 1000

  val primePairs: Set[(Int, Int)] = {
    val pairs = for {
      i <- 0 to upperLimit
      j <- 0 until i
      p = primes(j)
      q = primes(i)
      ps = p.toString
      qs = q.toString
      if isPrime((ps+qs).toInt)
      if isPrime((qs+ps).toInt)
    } yield (primes(j), primes(i))

    pairs.toSet
  }

  def concatsToPrime(p: Int, q: Int): Boolean = {
    primePairs.contains((p min q, p max q))
  }

  def grow(oldPrimes: Set[Int]): Seq[Set[Int]] = {
    primes
      .take(upperLimit)
      .filter(p => oldPrimes.forall(concatsToPrime(_, p)))
      .map(oldPrimes + _ )
  }

  val initial = primePairs.toSeq.map { case (a, b) => Set(a, b) }

  def growTo(n: Int): Seq[Set[Int]] = {
    if (n == 2) initial
    else growTo(n-1).flatMap(grow).distinct // important pruning!
  }

  val ans = growTo(4).map(_.sum).minOption

  println(ans)
}
