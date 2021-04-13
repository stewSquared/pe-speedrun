@main def p009(): Unit = {
  def isPythagorean(a: Int, b: Int, c: Int): Boolean =
    a*a + b*b == c*c

  val triples = for {
    a <- 1 to 998
    b <- 1 until a
    c = 1000 - a - b
    if isPythagorean(a, b, c)
  } yield a * b * c

  // assert(triples.length == 1)

  val ans = triples.head

  println(ans)
}
