@main def p013(): Unit = {
  val numbers = io.Source.fromFile("p013.txt").getLines.map(BigInt(_))

  val ans = numbers.sum.toString.take(10)

  println(ans)
}
