import fractions.Fraction
import math.Fractional.Implicits._

@main def p065(): Unit = {
  def continuedFraction(whole: Int, parts: Seq[Int]): Fraction = {
    Fraction(whole, 1) + parts.foldRight(Fraction(0, 1))((a, f) => (Fraction(a, 1) + f).reciprocal)
  }

  def eConvergent(n: Int): Fraction = {
    require(n > 0, "convergents are 1-indexed")
    val parts = LazyList.from(1).flatMap(k => Seq(1, 2*k, 1))
    continuedFraction(2, parts.take(n - 1))
  }

  val ans = eConvergent(100).numerator.toString.map(_.asDigit).sum

  println(ans)
}
