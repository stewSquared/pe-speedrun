@main def p010(): Unit = {
  val ans = algs.primesUntil(2_000_000).map(_.toLong).sum

  println(ans)
}
