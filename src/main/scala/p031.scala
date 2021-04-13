@main def p031(): Unit = {

  def countWays(value: Int, coins: List[Int]): Int = {
    if (value < 0) 0
    else if (value == 0) 1
    else coins match {
      case Nil => 0
      case c :: cs => countWays(value, cs) + countWays(value - c, coins)
    }
  }

  val ans = countWays(200, List(200, 100, 50, 20, 10, 5, 2, 1))

  println(ans)
}
