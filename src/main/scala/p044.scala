object p044 extends App {

  def pentagonalNumber(n: Int): Int = n * (3*n - 1) / 2

  val pents = LazyList.from(1).map(pentagonalNumber)

  def isPentagonal(n: Int): Boolean =
    pents
      .dropWhile(_ < n)
      .takeWhile(_ <= n)
      .nonEmpty

  val differences = pents.filter { diff =>
    pents.zip(pents.drop(1))
      .map { case (pent, next) => (pent, next - pent) }
      .takeWhile { case (pent, d) => d <= diff }
      .map { case (pent, _) => pent}
      .exists { pj =>
        val pk = pj + diff
        isPentagonal(pk) && isPentagonal(pj + pk)
      }
  }

  // val differences = for {
  //   d <- pents
  //   pj <- pents.zip(petagonals.drop(1)).map(_ - _).takeWhile(_ <= d).map(_._1) if isPentagonal(d + pj)

  // } yield (d, pj)

  val ans = differences.head

  println(ans)
}
