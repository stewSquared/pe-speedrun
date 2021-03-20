object p032 extends App {

  val pairs = "123456789".combinations(5)
    .flatMap(_.permutations)
    .flatMap(p => List(1,2).map(p.splitAt))
    .map { case (aDigits, bDigits) => (aDigits.toInt, bDigits.toInt)}

  def isPandigital(a: Int, b: Int): Boolean = {
    (a.toString + b.toString + (a*b).toString).sorted == "123456789"
  }

  val ans = pairs.collect {
    case (a, b) if isPandigital(a, b) => a * b
  }.distinct.sum

  println(ans)
}
