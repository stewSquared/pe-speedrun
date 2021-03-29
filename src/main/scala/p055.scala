object p055 extends App {
  def isPalindrome(n: BigInt): Boolean = {
    n.toString == n.toString.reverse
  }

  def reverseSums(n: BigInt): Iterator[BigInt] = {
    Iterator.iterate(n)(n => n + BigInt(n.toString.reverse))
  }

  def isLychrel(n: BigInt): Boolean = {
    ! reverseSums(n).drop(1).take(50).exists(isPalindrome)
  }

  val ans = (1 until 10_000).count(isLychrel)

  println(ans)
}
