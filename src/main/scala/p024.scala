object p024 extends App {
  val ans = (0 to 9).permutations.drop(999_999).next.mkString

  println(ans)
}
