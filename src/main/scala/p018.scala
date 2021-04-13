@main def p018(): Unit = {
  def maximumPathSum(rows: Seq[Seq[Int]]): Int = {
    rows.reduceRight[Seq[Int]] { case (nextRow, acc) =>
      acc.init.zip(acc.tail).zip(nextRow).map {
        case ((left, right), next) => left.max(right) + next
      }
    }.head
  }

  val tri = io.Source.fromFile("p018.txt")
    .getLines()
    .map(_.split(" ").map(_.toInt).toSeq)
    .toSeq

  val ans = maximumPathSum(tri)

  println(ans)
}
