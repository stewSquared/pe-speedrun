object p003 extends App {
  def factors(n: Long): List[Int] = {
    def loop(n: Long, k: Int): List[Int] = {
      if (n == 1) Nil
      else if (n % k == 0) k :: loop(n/k, k)
      else loop(n, k+1)
    }
    loop(n, k = 2)
  }

  val ans = factors(600851475143L).last

  println(ans)
}
