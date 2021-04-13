@main def p062(): Unit = {
  def cubes: Iterator[Long] = Iterator.from(1).map[Long](_.toLong).map(n => n*n*n)

  val ans = cubes
    .takeWhile(_ < 1_000_000_000_000L)
    .toSeq
    .groupBy(_.toString.sorted)
    .values.filter(_.size == 5)
    .flatten.min

  println(ans)
}
