object p015 extends App {

  def factorial(n: BigInt): BigInt = (BigInt(2) to n).product

  def choose(n: Int, k: Int): BigInt =
    factorial(n) / (factorial(k) * factorial(n - k))

  val ans = choose(40, 20)

  println(ans)
}
