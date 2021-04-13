import algs.primeFactors

@main def p047(): Unit = {

  def fourPrimeFactors(n: Int): Boolean = {
    primeFactors(n).distinct.length == 4
  }

  val ans = Iterator.from(1).sliding(4).collectFirst {
    case numbers if numbers.forall(fourPrimeFactors) => numbers.head
  }.get

  println(ans)
}
