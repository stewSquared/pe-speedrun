@main def p028(): Unit = {

  def cornerSum(side: Int): Int = {
    require(side % 2 == 1)
    require(side >= 3)

    val start = (side - 2) * (side - 2)

    start * 4 + (side - 1) * 10
  }

  val ans = 1 + (3 to 1001 by 2).map(cornerSum).sum

  println(ans)
}
