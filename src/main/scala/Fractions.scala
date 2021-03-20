package fractions

import algs.gcf

case class Fraction(numerator: Int, denominator: Int) {
  lazy val simplified: Fraction = {
    val k = gcf(numerator, denominator)
    Fraction(numerator / k, denominator / k)
  }

  override def equals(other: Any) = other match {
    case other: Fraction =>
      val numEqual = other.simplified.numerator == this.simplified.numerator
      val denEqual = other.simplified.denominator == this.simplified.denominator
      numEqual && denEqual
    case _ => false
  }

  def multiply(that: Fraction): Fraction =
    Fraction(this.numerator * that.numerator, this.denominator * that.denominator).simplified
}
