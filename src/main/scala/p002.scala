object p002 extends App {
  def fibs: LazyList[Int] = {
    def loop(a: Int, b: Int): LazyList[Int] = a #:: loop(b, a + b)

    loop(0, 1)
  }

  val ans = fibs
    .filter(_ % 2 == 0)
    .takeWhile(_ < 4000000)
    .sum

  println(ans)
}
