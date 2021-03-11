object p019 extends App {

  def daysInMonth(month: Int, year: Int): Int = month match {
    case 1 => 31
    case 2 =>
      if (year % 400 == 0 || (year % 4 == 0 && year % 100 != 0)) 29 else 28
    case 3 => 31
    case 4 => 30
    case 5 => 31
    case 6 => 30
    case 7 => 31
    case 8 => 31
    case 9 => 30
    case 10 => 31
    case 11 => 30
    case 12 => 31
  }

  def next(day: Int, month: Int, year: Int): (Int, Int, Int) = {
    val nextDay = (day + daysInMonth(month, year)) % 7
    val nextMonth = if (month == 11) 12 else (month + 1) % 12
    val nextYear = if (month == 12) year + 1 else year
    (nextDay, nextMonth, nextYear)
  }

  def firstDays = LazyList
    .iterate((1, 1, 1900))(Function.tupled(next _))

  val ans = firstDays
    .dropWhile { case (_, _, year) => year < 1901 }
    .takeWhile { case (_, _, year) => year < 2001 }
    .count { case (day, _, _) => day == 0 }

  println(ans)
}
