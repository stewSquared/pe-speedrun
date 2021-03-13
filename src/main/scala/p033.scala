object p033 extends App {
  case class Frac(numerator: Int, denominator: Int) {
    lazy val simplified: Frac = {
      val k = gcf(numerator, denominator)
      Frac(numerator / k, denominator / k)
    }

    override def equals(other: Any) = other match {
      case other: Frac =>
        val numEqual = other.simplified.numerator == this.simplified.numerator
        val denEqual = other.simplified.denominator == this.simplified.denominator
        numEqual && denEqual
      case _ => false
    }

    def multiply(that: Frac): Frac =
      Frac(this.numerator * that.numerator, this.denominator * that.denominator).simplified
  }

  def gcf(a: Int, b: Int): Int = {
    if (a == b) a
    else if (a < b) gcf(b, a)
    else gcf(a - b, b)
  }

  def removals(digit: Int, number: Int, from: Int = 0): List[Int] = {
    val index = number.toString.map(_.asDigit).indexOf(digit, from)
    if (number < 10) Nil
    else if (index == -1) Nil
    else {
      val (left, right) = number.toString.splitAt(index)
      val newNumber = (left ++ right.drop(1)).toInt
      if (newNumber == 0) removals(digit, number, index+1)
      else newNumber :: removals(digit, number, index+1)
    }
  }

  def isCurious(f: Frac): Boolean = {
    val commonDigits = f.numerator.toString.intersect(f.denominator.toString).map(_.asDigit)

    commonDigits.exists { digit =>
      val fracs = for {
        n <- removals(digit, f.numerator)
        d <- removals(digit, f.denominator)
      } yield Frac(n,d)
      fracs.exists { f2 =>
        f2 == f && (f.numerator != f2.numerator * 10)
      }
    }
  }

  def fractions: LazyList[Frac] = {
    def fracsOfLength(length: Int) = {
      for {
        d <- math.pow(10, length - 1).toInt until math.pow(10, length).toInt
        n <- 1 until d
      } yield Frac(n, d)
    }
    LazyList.from(2).flatMap(l => fracsOfLength(l))
  }

  val curiousFracs = fractions.filter(isCurious).take(4)

  val ans = curiousFracs.reduce(_ multiply _).denominator

  println(ans)
}
