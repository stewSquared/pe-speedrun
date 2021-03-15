object p045 extends App {

  def triangleNumber(n: Int): Int = n * (n+1) / 2

  def pentagonalNumber(n: Int): Int = n * (3*n - 1) / 2

  def hexagonalNumber(n: Int): Int = n * (2*n - 1)

  // def tris = LazyList.from(1).map(triangleNumber)


  // def trips(tris: LazyList[Int], pents: LazyList[Int], hexes: LazyList[Int]): LazyList[Int] = {
  //   (tris, pents, hexes) match {
  //     case (t#::ts, p#::ps, h#::hs) if t == p && p == h => t #:: trips(ts, ps, hs)
  //     case (t#::ts, p#::_, _) if t < p => trips(ts, pents, hexes)
  //     case (_, p#::ps, h#::_) if p < h => trips(tris, ps, hexes)
  //     case (_, _, h#::hs) => trips(tris, pents, hs)
  //   }
  // }

  // Note that every hexagonal number is already a triangular number

  def merge(pents: LazyList[Int], hexes: LazyList[Int]): LazyList[Int] = {
    (pents, hexes) match {
      case (p#::ps, h#::_) if p < h => merge(ps, hexes)
      case (p#::_, h#::hs) if p > h => merge(pents, hs)
      case (p#::ps, h#::hs) if p == h => p #:: merge(ps, hs)
    }
  }

  val ans = merge(
    pents = LazyList.from(1).map(pentagonalNumber),
    hexes = LazyList.from(1).map(hexagonalNumber)
  ).drop(2).head

  println(ans)
}
