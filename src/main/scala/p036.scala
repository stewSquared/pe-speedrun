@main def p036(): Unit = {

  def isBinPalindrome(n: Int): Boolean = {
    val s = n.toBinaryString
    s == s.reverse
  }

  val palindromes = {
    (1 to 1000).flatMap { n =>
      val s = n.toString
      List((s + s.reverse.tail), (s + s.reverse)).map(_.toInt)
    }
  }

  val ans = palindromes.filter(isBinPalindrome).sum

  println(ans)
}
