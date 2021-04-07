import algs.isPrime

object p058 extends App {

  def sideLengths = Iterator.from(1, 2)

  def corners(sideLength: Int): Iterator[Int] = {
    Iterator.from(sideLength*sideLength, -(sideLength - 1)).drop(1).take(3)
  }

  def ratios: Iterator[(Int, Int)] = sideLengths.drop(1).scanLeft((0, 1)){
    case ((primeCount, cornerCount), sideLength) =>
      (primeCount + corners(sideLength).count(isPrime), cornerCount + 4)
  }

  val (sideLength, (primeCount, cornerCount)) = sideLengths.zip(ratios)
    .drop(1) // disregard the 0% edge case for side length of 1
    .dropWhile { case (_, (primeCount, cornerCount)) =>
      primeCount.toDouble/cornerCount >= 0.10
    }.next

  println(s"ratio: ${primeCount.toDouble / cornerCount}")

  val ans = sideLength

  println(ans)
}
