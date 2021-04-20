package algs

import scala.collection.immutable.NumericRange
import scala.math.Integral.Implicits.infixIntegralOps
import scala.math.Ordering.Implicits.infixOrderingOps
import scala.math.sqrt

def factorial(n: Int): BigInt =
  (2 to n).foldLeft(BigInt(1))(_ * _)

def lcm[N : Integral](a: N, b: N): N =
  a * b / gcf(a, b)

def gcf[N : Integral](a: N, b: N): N =
  if (b == 0) a.abs
  else gcf(b, a % b)

def properDivisors[N : Integral](n: N): List[N] = {
  val factors = primeFactors(n)
  for {
    k <- (0 until factors.length).toList
    ps <- factors.combinations(k)
  } yield ps.product
}

def primeFactors[N](n: N)(using N: Integral[N]): List[N] = {
  require(n != N.zero, "0 has infinite factors")
  def loop(n: N, k: N): List[N] = {
    if (n == N.one) Nil
    else if (n % k == N.zero) k :: loop(n/k, k)
    else loop(n, k + N.one)
  }
  loop(n.abs, k = N.fromInt(2))
}

def primesUntil(n: Int): collection.BitSet = {
  val isPrime = new collection.mutable.BitSet(n)

  // optimize sieve by skipping even cases
  (3 until n by 2).foreach(isPrime += _)
  isPrime += 2

  for {
    m <- 3 to sqrt(n).toInt by 2 if isPrime(m)
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

