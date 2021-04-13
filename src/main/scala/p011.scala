@main def p011(): Unit = {

  val rows: List[List[Int]] = io.Source.fromFile("p011.txt")
    .getLines
    .map(_.split(" ").map(_.toInt).toList)
    .toList

  def diag(rows: List[List[Int]]): List[List[Int]] = {
    val padding = List.fill(rows.length - 1)(1)

    rows.zipWithIndex.map {
      case (row, i) =>
        val (left, right) = padding.splitAt(i)
        left ++ row ++ right
    }.transpose
  }

  def greatestProduct(length: Int): Int = {
    def greatest(rows: List[List[Int]]): Int = {
      rows.map { r =>
        r.sliding(length).map(_.product).max
      }.max
    }

    val horizontal = greatest(rows)
    val vertical = greatest(rows.transpose)
    val diagUp = greatest(diag(rows))
    val diagDown = greatest(diag(rows.reverse))

    horizontal max vertical max diagUp max diagDown
  }

  val ans = greatestProduct(4)

  println(ans)
}
