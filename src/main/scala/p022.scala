object p022 extends App {

  val names = io.Source
    .fromFile("p022.txt")
    .getLines
    .mkString
    .split(',')
    .map(_.drop(1).dropRight(1))

  def alphabeticalValue(s: String): Int = s.map(_ - 64).sum

  val ans = names
    .sorted
    .map(alphabeticalValue)
    .zipWithIndex
    .map { case (value, index) => value * (index+1) }
    .sum

  println(ans)
}
