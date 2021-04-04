import algs.primesUntil

object p051 extends App {

  val isPrime = primesUntil(1_000_000)

  def replacements(number: Int, digit: Char): Seq[Int] = {
    "1234567890".map { (replacement: Char) =>
      number.toString.replace(digit, replacement).toInt
    }.filter(_.toString.length == number.toString.length)
  }

  def primeFamilies(n: Int): Seq[Seq[Int]] = {
    n.toString.distinct.map { (digit: Char) =>
      replacements(n, digit).filter(isPrime)
    }.filter(_.nonEmpty)
  }

  def hasFamilyOf8(p: Int): Boolean =
    primeFamilies(p).exists(_.length == 8)

  val ans = isPrime.find(hasFamilyOf8)

  println(ans)
}
