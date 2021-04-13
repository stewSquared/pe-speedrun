@main def p006(): Unit = {

  def sumOfSquares(n: Int): Int = {
    (1 to n).map(n => n*n).sum
  }

  def squareOfSum(n: Int): Int = {
    val s = (1 to n).sum
    s*s
  }

  val ans = squareOfSum(100) - sumOfSquares(100)

  println(ans)
}
