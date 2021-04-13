@main def p017(): Unit = {

  def numberToWords(n: Int): String = n match {
    case 0 => "zero"
    case 1 => "one"
    case 2 => "two"
    case 3 => "three"
    case 4 => "four"
    case 5 => "five"
    case 6 => "six"
    case 7 => "seven"
    case 8 => "eight"
    case 9 => "nine"
    case 10 => "ten"
    case 11 => "eleven"
    case 12 => "twelve"
    case 13 => "thirteen"
    case 14 => "fourteen"
    case 15 => "fifteen"
    case 16 => "sixteen"
    case 17 => "seventeen"
    case 18 => "eighteen"
    case 19 => "nineteen"
    case 20 => "twenty"
    case 30 => "thirty"
    case 40 => "forty"
    case 50 => "fifty"
    case 60 => "sixty"
    case 70 => "seventy"
    case 80 => "eighty"
    case 90 => "ninety"
    case ty if (21 <= ty && ty <= 99) =>
      val remainder = ty % 10
      val tens = ty - remainder
      numberToWords(tens) + (
        if (remainder == 0) ""
        else numberToWords(remainder)
      )
    case h if (100 <= h && h <= 999) => {
      val hundreds = (n % 1000) / 100
      val remainder = h % 100
      numberToWords(hundreds) + "hundred" + (
        if (remainder == 0) ""
        else "and" + numberToWords(remainder)
      )
    }
    case 1000 => "onethousand"
    case _ => throw new Exception("larger than 1000")
  }

  val ans = (1 to 1000).map(numberToWords(_).length).sum

  println(ans)
}
