import algs.factorial

@main def p020(): Unit = {

  val ans = factorial(100).toString.map(_.asDigit).sum

  println(ans)
}
