object p042 extends App {
  def triangleNumber(n: Int): Int = n * (n+1) / 2

  val triangleNumbers = LazyList.from(1).map(triangleNumber)

  def isTriangular(n: Int): Boolean =
    triangleNumbers.takeWhile(_ <= n).contains(n)

  def alphabeticalValue(s: String): Int = s.map(_ - 64).sum

  val words = io.Source
    .fromFile("p042.txt")
    .getLines
    .mkString
    .split(',')
    .map(_.drop(1).dropRight(1))

  val ans = words.map(alphabeticalValue).count(isTriangular)

  println(ans)
}
