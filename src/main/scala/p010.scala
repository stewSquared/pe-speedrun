object p010 extends App {
  val ans = algs.primesUntil(2_000_000).map(_.toLong).sum

  println(ans)
}
