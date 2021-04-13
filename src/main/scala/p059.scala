@main def p059(): Unit = {
  def decrypt(message: Seq[Char], key: Seq[Char]): String = {
    message
      .zip(Iterator.continually(key).flatten)
      .map[Char]((m, k) => (m^k).toChar)
      .mkString
  }

  def keys = for {
    a <- 'a' to 'z'
    b <- 'a' to 'z'
    c <- 'a' to 'z'
  } yield Seq(a, b, c)

  val cipherText = io.Source.fromFile("p059.txt")
    .getLines().next
    .split(",")
    .map(_.toInt.toChar)
    .toSeq

  val original = keys
    .map(decrypt(cipherText, _))
    .find(_.containsSlice("Euler"))
    .get

  println(original)

  val ans = original.map(_.toInt).sum

  println(ans)
}
