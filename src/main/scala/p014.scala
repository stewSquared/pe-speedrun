@main def p014(): Unit = {

  def collatz(n: Long): Long = {
    if (n%2 == 0) n/2
    else 3*n + 1
  }

  val memo = collection.mutable.Map(1L -> 1)

  def distance(n: Long): Int = {
    memo.getOrElseUpdate(n, 1 + distance(collatz(n)))
  }

  val ans = (1L until 1_000_000).maxBy(distance)

  println(ans)
}
