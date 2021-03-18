object p047 extends App {

  def fourPrimeFactors(n: Int): Boolean = {
    algs.factors(n).distinct.length == 4
  }

  val ans = Iterator.from(1).sliding(4).collectFirst {
    case numbers if numbers.forall(fourPrimeFactors) => numbers.head
  }.get

  println(ans)
}
