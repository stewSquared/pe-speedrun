object p050 extends App {
  val primes = algs.primesUntil(1_000_000).toVector

  def prefixes = primes.scanLeft(0)(_ + _)

  def sumOfSlice(i: Int, j: Int): Int = {
    prefixes(j) - prefixes(i)
  }

  assert(sumOfSlice(0,1) == 2, "optimized slice")
  assert(primes.slice(0,1).sum == 2, "built in slice")

  def longestSum(target: Int, index: Int): Option[Seq[Int]] = {
    def loop(i: Int, j: Int): Option[Seq[Int]] = {
      val sum = sumOfSlice(i, j)
      if (j == index) None
      else if (sum == target) Some(primes.slice(i, j))
      else if(sum > target) loop(i+1, j)
      else loop(i, j+1)
    }

    loop(0,1).filter(_.length > 1)
  }

  // TODO: be smarter about bounds for searching for optimization
  val primesToCheck = primes.zipWithIndex.reverse.take(10000)

  val sums = primesToCheck.flatMap { case (p, i) => longestSum(p, i) }

  val ans = sums.maxBy[Int](_.length).sum

  println(ans)
}
