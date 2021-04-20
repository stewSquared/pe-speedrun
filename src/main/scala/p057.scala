import scala.math.Fractional.Implicits.infixFractionalOps

import fractions.{Fraction, over}

@main def p057(): Unit = {

  val one = 1 over 1

  def expansions: Iterator[Fraction] =
    Iterator.iterate(3 over 2)(f => one + (one + f).reciprocal)

  lazy val ans = expansions.take(1000).count {
    case Fraction(n, d) => n.toString.length > d.toString.length
  }

  println(ans)
}
