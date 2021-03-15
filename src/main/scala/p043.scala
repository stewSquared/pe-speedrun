object p043 extends App {

  val primes = algs.erastothenes.take(7)

  val pandigitals = "0123456789"
    .permutations
    .map(_.toLong)
    .filter(_ > 999_999_999)

  val ans = pandigitals.filter { pan =>
    pan.toString.sliding(3).drop(1).zip(primes).forall {
      case (n, p) => n.toInt % p == 0
    }
  }.sum

  println(ans)
}
