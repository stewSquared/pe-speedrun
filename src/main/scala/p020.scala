import algs.factorial

object p020 extends App {

  val ans = factorial(100).toString.map(_.asDigit).sum

  println(ans)
}
