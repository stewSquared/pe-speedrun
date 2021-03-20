import math.sqrt

object p041 extends App {

  // insight: all 8 and 9 digit pandigital numbers are divisibly by 3

  val isPrime = algs.primesUntil(10_000_000).toSet

  val ans = "7654321".permutations.map(_.toInt).filter(isPrime).next

  println(ans)
}
