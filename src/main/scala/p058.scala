import algs.primesUntil

object p058 extends App {

  val upperLimit = 10_000_000

  val isPrime = primesUntil(upperLimit).toSet

  def sideLengths = Iterator.from(1, 2)

  def corners(sideLength: Int): Iterator[Int] = {
    Iterator.from(sideLength*sideLength, -(sideLength - 1)).drop(1).take(3)
  }

  def ratios: Iterator[(Int, Int)] = sideLengths.drop(1).scanLeft((0, 1)){
    case ((primeCount, cornerCount), sideLength) =>
      (primeCount + corners(sideLength).count(isPrime), cornerCount + 4)
  }

  // sideLengths.zip(ratios) take 100 foreach println

  val ans = sideLengths.zip(ratios)
    .drop(1) // disregard the 0% edge case for side length of 1
    .takeWhile { case (sideLength, _) => sideLength * sideLength <= upperLimit }
    .dropWhile { case (_, (primeCount, cornerCount)) =>
      primeCount.toDouble/cornerCount >= 0.10
    }.nextOption

  ans.map { case (sideLength, (primeCount, cornerCount)) =>
    val ans = sideLength
    println(s"ratio: ${primeCount.toDouble / cornerCount}")
    println(ans)
  }.getOrElse {
    println(s"could not find answer within: $upperLimit")
  }
}
