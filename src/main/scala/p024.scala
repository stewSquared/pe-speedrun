@main def p024(): Unit = {
  val ans = (0 to 9).permutations.drop(999_999).next.mkString

  println(ans)
}
