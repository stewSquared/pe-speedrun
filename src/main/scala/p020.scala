object p020 extends App {

  val ans = (BigInt(2) to 100).product.toString.map(_.asDigit).sum

  println(ans)
}
