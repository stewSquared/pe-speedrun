import fractions.Fraction

@main def p033(): Unit = {

  def curious(num: Int, den: Int): Boolean = {
    def commonDigits: String = s"$num".intersect(s"$den").distinct

    commonDigits.grouped(1).exists { (digit: String) =>
      // `replaceFirst` is fine since these are two-digit numbers
      val n = s"$num".replaceFirst(digit, "").toInt
      val d = s"$den".replaceFirst(digit, "").toInt
      Fraction(n,d) == Fraction(num, den)
    }
  }

  val fractions = {
    for {
      d <- 10 to 99 // "two digits"
      n <- 10 until d // "less than one in value"
      gcf = algs.gcf(n, d)
      coprime = gcf == 1
      trivial = gcf % 10 == 0
      if !(coprime || trivial) && curious(n, d)
    } yield Fraction(n, d)
  }

  val ans = fractions.product.denominator

  println(ans)
}
