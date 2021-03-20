import fractions.Fraction

object p033 extends App {

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

  def isCurious(f: Fraction): Boolean = {
    val commonDigits = f.numerator.toString.intersect(f.denominator.toString).map(_.asDigit)

    commonDigits.exists { digit =>
      val fracs = for {
        n <- removals(digit, f.numerator)
        d <- removals(digit, f.denominator)
      } yield Fraction(n,d)
      fracs.exists { f2 =>
        f2 == f && (f.numerator != f2.numerator * 10)
      }
    }
  }

  def fractions: LazyList[Fraction] = {
    def fracsOfLength(length: Int) = {
      for {
        d <- math.pow(10, length - 1).toInt until math.pow(10, length).toInt
        n <- 1 until d
      } yield Fraction(n, d)
    }
    LazyList.from(2).flatMap(l => fracsOfLength(l))
  }

  val curiousFractions = fractions.filter(isCurious).take(4)

  val ans = curiousFractions.reduce(_ multiply _).denominator

  println(ans)
}
