object p061 extends App {
  def fourDigit(g: Int => Int): List[Int] =
    Iterator.from(1).map(g).dropWhile(_ < 1000).takeWhile(_ < 10_000).toList

  val figurateSequences: List[List[Int]] = List(
    fourDigit(n => n * (n + 1) / 2),
    fourDigit(n => n * n),
    fourDigit(n => n * (3*n - 1) / 2),
    fourDigit(n => n * (2*n - 1)),
    fourDigit(n => n * (5*n - 3) / 2),
    fourDigit(n => n * (3*n - 2))
  )

  def search(prev: Int, sequences: List[List[Int]]): List[List[Int]] = {
    sequences match {
      case Nil => List(List(prev))
      case ps :: remaining => {
        ps.filter(_ / 100 == prev % 100)
          .flatMap(p => search(p, remaining).map(prev :: _))
      }
    }
  }

  val chains: List[List[Int]] = (for {
    t <- figurateSequences.head
    sequences <- figurateSequences.tail.permutations.toList
    chain <- search(t, sequences)
  } yield chain)

  val ans = chains.filter(c => c.last % 100 == c.head / 100).head

  println(ans)
  println(ans.sum)
}
