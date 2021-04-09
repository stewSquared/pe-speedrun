import scala.collection.immutable.NumericRange
import scala.math.Integral.Implicits.infixIntegralOps
import scala.math.Ordering.Implicits.infixOrderingOps
import scala.math.sqrt

package object algs {

  def factorial(n: Int): BigInt =
    (2 to n).foldLeft(BigInt(1))(_ * _)

  def lcm[N : Integral](a: N, b: N): N = {
    a * b / gcf(a, b)
  }

  def gcf[N : Integral](a: N, b: N): N = {
    if (b == 0) a.abs
    else gcf(b, a % b)
  }

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

  def primesUntil(n: Int): collection.BitSet = {
    val isPrime = new collection.mutable.BitSet(n)

    // optimize sieve by skipping even cases
    (3 until n by 2).foreach(isPrime += _)
    isPrime += 2

    for {
      m <- 3 until sqrt(n).toInt by 2
      k <- m*3 until n by m*2
    } {
      isPrime -= k
    }

    isPrime
  }

  def isPrime(n: Int): Boolean = {
    if (n < 2) false
    else {
      val root = sqrt(n)
      (!root.isWhole
        && (2 to root.toInt).forall(n % _ != 0))
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
