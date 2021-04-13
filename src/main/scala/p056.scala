@main def p056(): Unit = {
  val powers = for {
    a <- 1 until 100
    b <- 1 until 100
  } yield BigInt(a).pow(b)

  val ans = powers.map(_.toString.map(_.asDigit).sum).max

  println(ans)
}
