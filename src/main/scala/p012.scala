import math.{sqrt, floor}

object p012 extends App {

  val triangularNumbers =
    LazyList.from(1).scan(0)(_ + _).tail


  def perfectSquare(n: Int) = {
    val r = floor(sqrt(n))
    r * r == n
  }

  val numDivisors = triangularNumbers.map { n =>
    val divisorsUnderRoot = (1 until floor(sqrt(n)).toInt).filter(n % _ == 0)
    val numDivisors = divisorsUnderRoot.length * 2 + (if (perfectSquare(n)) 1 else 0)
    n -> numDivisors
  }

  val ans = numDivisors.collectFirst {
    case (n, count) if count > 500 => n
  }.get

  println(ans)
}
