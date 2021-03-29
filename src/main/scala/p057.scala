import fractions.Fraction

object p057 extends App {
  def expansions: Iterator[Fraction] =
    Iterator.iterate(Fraction(3,2))(f =>
      Fraction(1, 1).add((Fraction(1,1).add(f)).reciprocal)
    )

  lazy val ans = expansions.take(1000).count {
    case Fraction(n, d) => n.toString.length > d.toString.length
  }

  println(ans)
}
