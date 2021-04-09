import scala.math.Fractional.Implicits.infixFractionalOps

import fractions.Fraction

object p057 extends App {
  val one = Fraction(1, 1)

  def expansions: Iterator[Fraction] =
    Iterator.iterate(Fraction(3, 2))(f => one + (one + f).reciprocal)

  lazy val ans = expansions.take(1000).count {
    case Fraction(n, d) => n.toString.length > d.toString.length
  }

  println(ans)
}
