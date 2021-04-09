package fractions

import algs.gcf

case class Fraction private (numerator: BigInt, denominator: BigInt) {

  override def toString: String = s"$numerator/$denominator"

  def add(that: Fraction): Fraction = {
    val n = this.numerator * that.denominator + that.numerator * this.denominator
    val d = this.denominator * that.denominator
    Fraction(n, d)
  }

  def negate: Fraction = new Fraction(-numerator, denominator)

  def subtract(that: Fraction): Fraction = this.add(that.negate)

  def multiply(that: Fraction): Fraction = {
    val n = this.numerator * that.numerator
    val d = this.denominator * that.denominator
    Fraction(n, d)
  }

  def reciprocal: Fraction = new Fraction(this.denominator, this.numerator)

  def divide(that: Fraction): Fraction = this.multiply(that.reciprocal)

  def toDouble = numerator.toDouble / denominator.toDouble

  def mixedNumber: (BigInt, Fraction) = {
    val (whole, rem) = numerator /% denominator
    (whole, this.copy(numerator = rem))
  }

  def isWhole: Boolean = denominator == 1
}

object Fraction {
  def apply(n: BigInt, d: BigInt): Fraction = {
    val k = gcf(n, d)
    if (d > 0) new Fraction(n / k, d / k)
    else if (d < 0) new Fraction(-n / k, -d / k)
    else throw new ArithmeticException("/ by zero (Fraction denominator)")
  }

  given Fractional[Fraction] with {
    def plus(a: Fraction, b: Fraction): Fraction = a.add(b)
    def minus(a: Fraction, b: Fraction): Fraction = a.subtract(b)
    def times(a: Fraction, b: Fraction): Fraction = a.multiply(b)
    def div(a: Fraction, b: Fraction): Fraction = a.divide(b)
    def negate(a: Fraction): Fraction = a.negate
    def fromInt(a: Int): Fraction = Fraction(a, 1)
    def parseString(str: String): Option[Fraction] = str match {
      case s"$num/$den" => util.Try(Fraction(BigInt(num), BigInt(den))).toOption
      case _ => None
    }
    def toInt(a: Fraction): Int = a.mixedNumber._1.toInt
    def toLong(a: Fraction): Long = a.mixedNumber._1.toLong
    def toFloat(a: Fraction): Float = a.toDouble.toFloat
    def toDouble(a: Fraction): Double = a.toDouble

    def compare(a: Fraction, b: Fraction): Int = a.subtract(b).numerator.toInt
  }
}
