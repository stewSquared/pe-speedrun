object p032 extends App {

  val pairs = "123456789".combinations(5)
    .flatMap(_.permutations)
    .flatMap(p => List(1,2).map(p.splitAt))
    // .map { case (aDigits, bDigits) => aDigits.toInt, bDigits.toInt}

  // def isPandigital(aDigits: String, bDigits: String): Boolean = {
  //   val product = aDigits.toInt * bDigits.toInt
  // }

  val ans = pairs.map {
    case (aDigits, bDigits) =>
      val a = aDigits.mkString.toInt
      val b = bDigits.mkString.toInt

      val isPandigital = (aDigits.mkString + bDigits.mkString + (a*b).toString).sorted == "123456789"
      (a*b, isPandigital)
  }.collect {
    case (product, isPandigital) if isPandigital => product
  }.distinct.sum

  println(ans)
}
