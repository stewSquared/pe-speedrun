@main def p001(): Unit = {
  val ans = (1 until 1000).filter(n => n%5 == 0 ||n%3 == 0).sum
  println(ans)
}
