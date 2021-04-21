import math.sqrt
import math.Fractional.Implicits.given
import BigDecimal.defaultMathContext as mc

import fractions.{Fraction, over}

@main def p066(): Unit = {
  case class State(num: Int, radical: Int, offset: Int)

  def next(state: State): (Int, State) = {
    import state.*

    val nextNum = (radical - offset*offset) / num

    val whole = (sqrt(radical).toInt + offset) / nextNum

    val nextOffset = sqrt(radical).toInt - ((sqrt(radical).toInt + offset) % nextNum)

    whole -> state.copy(num = nextNum, offset = nextOffset)
  }

  def continuedFraction(whole: Int, parts: Seq[Int]): Fraction =
    (whole over 1) + parts.foldRight(0 over 1) {
      (a, f) => ((a over 1) + f).reciprocal
    }

  def convergents(radical: Int): Iterator[Fraction] = {
    val whole = sqrt(radical).toInt
    Iterator
      .unfold(State(1, radical, whole))(s => Some(next(s)))
      .scanLeft(Vector.empty[Int])(_ :+ _)
      .map(parts => continuedFraction(whole, parts))
  }

  def minX(d: Int): Option[BigInt] = {
    if (sqrt(d).isWhole) None
    else {
      convergents(d).find { f =>
        val x = f.numerator
        val y = f.denominator
        x*x - d*y*y == 1
      }.map(_.numerator)
    }
  }

  val ans = (2 to 1000).maxBy(minX)

  println(ans)
}
