import sequences.fibonacci

@main def p025(): Unit = {

  val ans = fibonacci.indexWhere(_.toString.length == 1000)

  println(ans)
}
