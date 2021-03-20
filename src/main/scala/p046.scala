import algs.primesUntil

object p046 extends App {

  val upperLimit = 10000

  val primes = primesUntil(upperLimit).drop(1)

  val isPrime = primes.toSet

  def satisfiesGoldbach(odd: Int): Boolean = {
    require(odd%2 == 1)
    primes
      .takeWhile(_ < odd)
      .exists(p => math.sqrt((odd - p) / 2).isWhole)
  }

  val oddComposites = (3 until upperLimit by 2)
    .filterNot(isPrime)

  val ans = oddComposites
    .filterNot(satisfiesGoldbach)
    .headOption.get

  println(ans)
}
