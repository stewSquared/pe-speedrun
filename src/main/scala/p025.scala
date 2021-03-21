import sequences.fibonacci

object p025 extends App {

  val ans = fibonacci.indexWhere(_.toString.length == 1000)

  println(ans)
}
