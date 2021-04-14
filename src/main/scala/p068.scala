import algs.factorial

@main def p068(): Unit = {

  def nodeLines(outer: Seq[Int], inner: Seq[Int]): Seq[Seq[Int]] = {
    outer.zip((inner :+ inner.head).sliding(2))
      .map(_ +: _)
  }

  val fiveGons = for {
    outer <- (10 to 7 by -1).permutations.map(6 +: _)
    inner <- (5 to 1 by -1).permutations
    lines = nodeLines(outer, inner)
    if lines.map(_.sum).distinct.sizeIs == 1
  } yield lines.flatten

  val ans = fiveGons.next.mkString

  println(ans)
}
