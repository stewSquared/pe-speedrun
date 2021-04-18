import fractions.Fraction

// TODO: 42 seconds. optimize

@main def p073(): Unit = {
  val fractions = for {
    d <- 1 to 12000
    n0 = (d / 3) + 1
    n1 = if (d%2 != 0) d / 2 else d / 2 - 1
    n <- n0 to n1
  } yield Fraction(n, d)

  val ans = fractions.distinct.length

  println(ans)
}
