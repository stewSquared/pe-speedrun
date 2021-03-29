package fractions

import scala.math.Integral.Implicits.infixIntegralOps

import algs.gcf

case class Fraction(numerator: BigInt, denominator: BigInt) {
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

  def multiply(that: Fraction): Fraction = {
    val n = this.numerator * that.numerator
    val d = this.denominator * that.denominator
    Fraction(n, d).simplified
  }

  def add(that: Fraction): Fraction = {
    val n = this.numerator * that.denominator + that.numerator * this.denominator
    val d = this.denominator * that.denominator
    Fraction(n, d).simplified
  }

  def reciprocal: Fraction = Fraction(this.denominator, this.numerator)

  def divide(that: Fraction): Fraction =
    this.multiply(that.reciprocal)

}
