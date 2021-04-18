import algs.primesUntil

@main def p072(): Unit = {

  def phiUntil(n: Int): Array[Int] = {
    val phi = (0 until n).toArray
    phi(1) = 0
    for {
      p <- primesUntil(n)
      k <- p until n by p
    } {
      phi(k) = phi(k) / p * (p-1)
    }
    phi
  }

  val ans = phiUntil(1_000_001).map(_.toLong).sum

  println(ans)
}
