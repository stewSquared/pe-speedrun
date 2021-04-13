import algs.lcm

@main def p005(): Unit = {
  val ans = (1 to 20).reduce(lcm)

  println(ans)
}
