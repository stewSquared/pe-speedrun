import algs.primesUntil

object p035 extends App {

  def circulations(n: Int): Seq[Int] = {
    def circulate(s: String): String = {
      (s.drop(1) ++ s.take(1))
    }
    def loop(newNumber: String): List[String] = {
      if (newNumber == n.toString) List(newNumber)
      else newNumber :: loop(circulate(newNumber))
    }
    loop(circulate(n.toString)).map(_.toInt)
  }

  val isPrime = primesUntil(1_000_000)

  def circular(p: Int): Boolean = {
    circulations(p).forall(isPrime)
  }

  val ans = isPrime.count(circular)

  println(ans)
}
