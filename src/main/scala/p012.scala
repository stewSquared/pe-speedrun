import math.sqrt
import sequences.triangulars

object p012 extends App {

  def perfectSquare(n: Int) = sqrt(n).isWhole

  val numDivisors = triangulars.map { n =>
    val divisorsUnderRoot = (1 until sqrt(n).toInt).filter(n % _ == 0).length
    val numDivisors = divisorsUnderRoot * 2 + (if (perfectSquare(n)) 1 else 0)
    n -> numDivisors
  }

  val ans = numDivisors.collectFirst {
    case (n, count) if count > 500 => n
  }.get

  println(ans)
}
