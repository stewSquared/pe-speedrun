import algs.properDivisors

object p021 extends App {

  def d(n: Int): Int = properDivisors(n).sum.toInt

  def amicable(n: Int): Boolean = {
    (d(d(n)) == n) && (d(n) != n)
  }

  val ans = (1 until 10000).filter(amicable).sum

  println(ans)
}
