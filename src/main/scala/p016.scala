object p016 extends App {
  val ans = BigInt(2).pow(1000).toString.map(_.asDigit).sum

  println(ans)
}
