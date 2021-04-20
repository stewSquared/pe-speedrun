import fractions.over

@main def p071(): Unit = {
  val fractions = for {
    d <- 1 to 1000000
    if d%7 != 0
    n = d * 3 / 7
  } yield n over d

  val ans = fractions.max

  println(ans.numerator)
}
