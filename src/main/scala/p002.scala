import sequences.fibonacci

object p002 extends App {

  val ans = fibonacci
    .filter(_ % 2 == 0)
    .takeWhile(_ < 4000000)
    .sum

  println(ans)
}
