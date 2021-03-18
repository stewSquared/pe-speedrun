object p045 extends App {

  def pentagonalNumber(n: Long): Long = n * (3*n - 1) / 2

  def hexagonalNumber(n: Long): Long = n * (2*n - 1)

  // note that all hexagonals are already triangle numbers

  // def merge(pents: LazyList[Int], hexes: LazyList[Int]): LazyList[Int] = {
  //   (pents, hexes) match {
  //     case (p#::ps, h#::_) if p < h => merge(ps, hexes)
  //     case (p#::_, h#::hs) if p > h => merge(pents, hs)
  //     case (p#::ps, h#::hs) if p == h => p #:: merge(ps, hs)
  //   }
  // }

  def isPentagonal(n: Long): Boolean = (1 + math.sqrt(1 + 24*n)) % 6 == 0.0

  val allThree = LazyList.from(1)
    .map(n => hexagonalNumber(n.toLong))
    .filter(isPentagonal)

  val ans = allThree.drop(2).head

  println(ans)
}
