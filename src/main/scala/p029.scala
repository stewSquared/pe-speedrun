object p029 extends App {
  val terms = for {
    a <- 2 to 100
    b <- 2 to 100
  } yield BigInt(a).pow(b)

  val ans = terms.distinct.length

  println(ans)
}
