object p025 extends App {

  def fibs: LazyList[BigInt] = {
    def loop(a: BigInt, b: BigInt): LazyList[BigInt] =
      a #:: loop(b, a + b)

    loop(BigInt(0), BigInt(1))
  }

  val ans = fibs.indexWhere(f => f.toString.length == 1000)

  println(ans)
}
