import sequences.{hexagonals, isPentagonal}

@main def p045(): Unit = {

  // note that all hexagonals are already triangle numbers

  val ans = hexagonals.filter(isPentagonal).drop(2).next

  println(ans)
}
