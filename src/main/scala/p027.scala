object p027 extends App {

  val primes = algs.primesUntil(100000).toSet

  def quadratic(a: Int, b: Int): Int => Int = {
    n => n*n + a*n + b
  }

  def consecutivePrimes(f: Int => Int): Int = {
    Stream.from(0).takeWhile(n => primes.contains(f(n))).length
  }

  val coeficients = for {
    a <- -999 to 999
    b <- -1000 to 1000
  } yield {
    (a,b)
  }

  val (a, b) = coeficients.maxBy{case (a, b) => consecutivePrimes(quadratic(a, b))}

  println(s"$a $b")

  val ans = a * b

  println(ans)
}
