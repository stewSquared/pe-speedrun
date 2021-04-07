object p063 extends App {
  def countNthPower(n: Int): Int = Iterator.from(1)
    .map(math.pow(_, n))
    .dropWhile(_ < math.pow(10, n-1))
    .takeWhile(_ < math.pow(10, n))
    .length

  val ans = (1 to 30).map(countNthPower).sum

  // TODO do some math that gets a better upper bounds that 30

  println(ans)
}
