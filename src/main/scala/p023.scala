import algs.properDivisors

object p023 extends App {

  def abundant(n: Int): Boolean = properDivisors(n).sum > n

  val upperLimit = 28123

  val abundantNumbers = (1 to upperLimit).filter(abundant).toSet

  def writableAsSum(n: Int): Boolean = {
    abundantNumbers
      .exists(a => abundantNumbers(n - a))
  }

  val ans = (1 to upperLimit).filterNot(writableAsSum).sum

  println(ans)
}
