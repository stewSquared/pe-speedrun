object p007 extends App {

  def primes: LazyList[Int] = {

    def merge(left: LazyList[Int], right: LazyList[Int]): LazyList[Int] = {
      (left, right) match {
        case (lh #:: lt, rh #:: rt) =>
          val next = lh min rh
          next #:: merge(
            if (next == lh) lt else left,
            if (next == rh) rt else right
          )
        case _ => left #::: right
      }
    }

    def mults(p: Int) = LazyList.from(p * p, p)

    def loop(n: Int, primes: LazyList[Int], composites: LazyList[Int]): LazyList[Int] = {
      (primes, composites) match {
        case (_, c #:: cs) if n == c => loop(n+1, primes, cs)
        case (p #:: ps, _) if n == p*p => loop(n, ps, merge(mults(p), composites))
        case _ => n #:: loop(n+1, primes, composites)
      }

    }

    lazy val p: LazyList[Int] = 2 #:: loop(3, p, LazyList.empty)
    p
  }

  val ans = primes(10000)

  println(ans)
}
