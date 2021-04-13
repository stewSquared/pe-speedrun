@main def p052(): Unit = {

  def sameDigits(x: Int, m: Int): Boolean = {
    x.toString.sorted == (x*m).toString.sorted
  }

  val ans = Iterator.from(1).filter { x =>
    (2 to 6).forall(sameDigits(x, _))
  }.next

  println(ans)
}
