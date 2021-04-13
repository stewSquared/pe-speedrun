import sequences.fibonacci

@main def p002(): Unit = {

  val ans = fibonacci
    .filter(_ % 2 == 0)
    .takeWhile(_ < 4000000)
    .sum

  println(ans)
}
