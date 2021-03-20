import algs.lcm

object p005 extends App {
  val ans = (1 to 20).reduce(lcm)

  println(ans)
}
