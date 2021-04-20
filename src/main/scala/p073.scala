@main def p073(): Unit = {

  val ans = (1 to 12000).map { d =>
    val n0 = (d / 3) + 1
    val n1 = if (d%2 != 0) d / 2 else d / 2 - 1

    (n0 to n1).count(algs.gcf(d, _) == 1)
  }.sum

  println(ans)
}
