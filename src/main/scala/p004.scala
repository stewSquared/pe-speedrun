object p004 extends App {
  def palindromic(n: Int): Boolean = n.toString.reverse == n.toString

  // def products: LazyList

  val products = for {
    m <- 999 to 100 by -1
    n <- m to 100 by -1
  } yield {
    m * n
  }

  val ans = products.filter(palindromic).max

  println(ans)
}
