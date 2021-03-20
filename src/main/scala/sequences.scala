import  math.sqrt

package object sequences {

  def fibonacci: LazyList[BigInt] = {
    def loop(a: BigInt, b: BigInt): LazyList[BigInt] =
      a #:: loop(b, a + b)

    loop(BigInt(0), BigInt(1))
  }

  def triangular(n: Int): Int = n*(n+1)/2

  def triangulars: Iterator[Int] = {
    Iterator.from(1).map(triangular)
  }

  // ((-1 + sqrt(1 + 8*n)) / 2).isWhole
  def isTriangular(n: Int): Boolean = sqrt(1 + 8*n).isWhole

  def pentagonal(n: Long): Long = n * (3*n - 1) / 2

  def isPentagonal(n: Long): Boolean = (1 + math.sqrt(1 + 24*n)) % 6 == 0.0

  def hexagonal(n: Long): Long = n * (2*n - 1)

  def hexagonals: Iterator[Long] = {
    Iterator.from(1).map(n => hexagonal(n.toLong))
  }
}
