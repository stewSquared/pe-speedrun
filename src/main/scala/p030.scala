object p030 extends App {

  val ans = (2 to 1_000_000).filter { n =>
    n == n.toString.map(c => math.pow(c.asDigit, 5).toInt).sum
  }.sum

  println(ans)
}
