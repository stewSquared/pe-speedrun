import sequences.isTriangular

@main def p042(): Unit = {

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
