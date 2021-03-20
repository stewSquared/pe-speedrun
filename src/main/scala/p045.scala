import sequences.{hexagonals, isPentagonal}

object p045 extends App {

  // note that all hexagonals are already triangle numbers

  val ans = hexagonals.filter(isPentagonal).drop(2).next

  println(ans)
}
