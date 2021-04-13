import syntax._

@main def p053(): Unit = {
  val binomials = for {
    n <- 1 to 100
    r <- 0 to n
  } yield n choose r

  val ans = binomials.count(_ > 1_000_000)

  println(ans)
}
