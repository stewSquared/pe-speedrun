import algs.erastothenes
import algs.isPrime

@main def p046(): Unit = {

  val primes = erastothenes.drop(1)

  def satisfiesGoldbach(odd: Int): Boolean = {
    require(odd%2 == 1)
    primes
      .takeWhile(_ < odd)
      .exists(p => math.sqrt((odd - p) / 2).isWhole)
  }

  def oddComposites = Iterator.from(3, 2).filterNot(isPrime)

  val ans = oddComposites.filterNot(satisfiesGoldbach).next

  println(ans)
}
