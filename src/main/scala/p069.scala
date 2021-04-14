import algs.erastothenes

@main def p069(): Unit = {

  val ans = erastothenes.scanLeft(1)(_ * _).takeWhile(_ <= 1_000_000).last

  println(ans)
}
