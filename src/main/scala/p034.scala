@main def p034(): Unit = {
  def factorial(n: Int): Int = (1 to n).product

  val ans = (10 until 10_000_000).filter(n => n.toString.map(c => factorial(c.asDigit)).sum == n).sum

  println(ans)
}
