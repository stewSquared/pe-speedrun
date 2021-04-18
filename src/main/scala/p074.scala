@main def p074(): Unit = {

  def factorialDigitSum(n: BigInt): BigInt = {
    n.toString.map(d => algs.factorial(d.asDigit)).sum
  }

  def factorialDigitSequence(n: BigInt): LazyList[BigInt] = {
    LazyList.iterate(n)(factorialDigitSum)
  }

  def countNonRepeating(n: Int): Int = {
    def loop[A](seen: List[A], rest: LazyList[A]): Int = {
      rest match {
        case r #:: rs if (seen.contains(r)) => seen.length
        case r #:: rs  => loop(r :: seen, rs)
        case _ => ???
      }
    }
    loop(Nil, factorialDigitSequence(n))
  }

  val ans = (1 to 1000000).count(countNonRepeating(_) == 60)

  println(ans)
}
