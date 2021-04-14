import math.sqrt

@main def p064(): Unit = {

  case class State(num: Int, radical: Int, offset: Int)

  def next(state: State): (Int, State) = {
    import state._

    val nextNum = (radical - offset*offset) / num

    val whole = (sqrt(radical).toInt + offset) / nextNum

    val nextOffset = sqrt(radical).toInt - ((sqrt(radical).toInt + offset) % nextNum)

    whole -> state.copy(num = nextNum, offset = nextOffset)
  }

  def period[T](seq: Seq[T]): Int = {
    val start = seq.head
    seq.tail.takeWhile(_ != start).length + 1
  }

  def states(radical: Int): LazyList[State] = {
    LazyList.iterate(State(1, radical, sqrt(radical).toInt)){
      case state => next(state)._2
    }
  }

  val ans = (2 to 10_000)
    .filterNot(n => sqrt(n).isWhole)
    .count(n => period(states(n))%2 == 1)

  println(ans)
}
