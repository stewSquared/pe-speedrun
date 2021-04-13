@main def p044(): Unit = {

  def pentagonalNumber(n: Int): Int = n * (3*n - 1) / 2

  def pents = LazyList.from(1).map(pentagonalNumber)

  // p = n(3n - 1) / 2

  // 3n^2 - n -2p = 0

  // n = (-(-1) +/- sqrt((-1)^2 - 4 * 3 * -2p)) / 2*3

  // n = (1 + sqrt(1 + 24p)) / 6

  def isPentagonal(n: Int): Boolean = (1 + math.sqrt(1 + 24*n)) % 6 == 0.0

  def differences = pents.filter { diff =>
    pents.take(math.sqrt(diff).toInt).exists { left =>
        val right = left + diff
        isPentagonal(right) && isPentagonal(left + right)
      }
  }

  val ans = differences.head

  println(ans)
}
