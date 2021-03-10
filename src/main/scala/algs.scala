package object algs {

  def erastothenes: LazyList[Int] = {
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

    def mults(p: Int) = LazyList.from(p * (p + 2), p * 2)

    def loop(n: Int, primes: LazyList[Int], composites: LazyList[Int]): LazyList[Int] = {
      (primes, composites) match {
        case (_, c #:: cs) if n == c => loop(n+2, primes, cs)
        case (p #:: ps, _) if n == p*p => loop(n+2, ps, merge(mults(p), composites))
        case _ => n #:: loop(n+2, primes, composites)
      }
    }

    lazy val primes: LazyList[Int] = 2 #:: 3 #:: loop(5, primes.tail, LazyList.empty)
    primes
  }

}
