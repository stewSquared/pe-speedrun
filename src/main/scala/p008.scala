object p008 extends App {
  val digits = io.Source.fromFile("p008.txt")
    .getLines
    .flatMap(_.map(_.asDigit.toLong))

  val ans = digits.sliding(13).map(_.product).max

  println(ans)
}
