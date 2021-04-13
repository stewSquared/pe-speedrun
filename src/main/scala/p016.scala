@main def p016(): Unit = {
  val ans = BigInt(2).pow(1000).toString.map(_.asDigit).sum

  println(ans)
}
