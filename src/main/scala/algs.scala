package object algs {

  def divisors(n: Long): Seq[Long] = {
    if (n > 1) {
      val primeFactors = factors(n)

      val primeFactorCombinations: Seq[Long] =
        (1 to primeFactors.length - 1).toSeq
          .flatMap(primeFactors.combinations(_).map(_.product))

      1L +: primeFactorCombinations
    } else Nil
  }

  def factors(n: Long): List[Int] = {
    def loop(n: Long, k: Int): List[Int] = {
      if (n == 1) Nil
      else if (n % k == 0) k :: loop(n/k, k)
      else loop(n, k+1)
    }
    loop(n, k = 2)
  }

  def primesUntil(n: Int): Seq[Int] = {
    val prime = Array.fill[Boolean](n)(true)

    for {
      m <- 2 until math.floor(math.sqrt(n)).toInt
      k <- m*2 until n by m if prime(k)
    } {
      prime(k) = false
    }

    prime.zipWithIndex.drop(2).collect {
      case (isPrime, i) if isPrime => i
    }
  }

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
