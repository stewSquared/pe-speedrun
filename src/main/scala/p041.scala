import math.sqrt
import algs.isPrime

object p041 extends App {

  // insight: all 8 and 9 digit pandigital numbers are divisibly by 3

  val ans = "7654321".permutations.map(_.toInt).filter(isPrime).next

  println(ans)
}
