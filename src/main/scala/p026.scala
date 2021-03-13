object p026 extends App {

  def cycleLength(n: Int): Int = {
    val remainders = LazyList.iterate(1 % n)(r => (r*10) % n).takeWhile(_ != 0)

    def findRepeats(remainders: LazyList[Int], seen: List[Int]): Int = {
      remainders match {
        case r #:: rs if(seen.contains(r)) => seen.indexOf(r) + 1
        case r #:: rs => findRepeats(rs, r :: seen)
        case _ => 0
      }
    }

    findRepeats(remainders, Nil)
  }

  val ans = (1 until 1000).maxBy(cycleLength)

  println(ans)
}
