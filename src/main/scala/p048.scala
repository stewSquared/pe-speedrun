@main def p048(): Unit = {

  def powers(n: Int): LazyList[Long] = LazyList.iterate(1L)(p => (p*n) % 10_000_000_000L)

  val ans = (1 to 1000).map(n => powers(n)(n)).sum % 10_000_000_000L

  println(ans)
}
