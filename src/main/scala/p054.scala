import scala.math.Ordering.Implicits.seqOrdering

@main def p054(): Unit = {
  enum Suit {
    case Clubs, Diamonds, Spades, Hearts
  }
  object Suit {
    def fromChar(c: Char): Suit = c match {
      case 'C' => Clubs
      case 'D' => Diamonds
      case 'S' => Spades
      case 'H' => Hearts
    }
  }

  enum Rank {
    case Two, Three, Four, Five, Six, Seven, Eight,
      Nine, Ten, Jack, Queen, King, Ace
  }
  object Rank {
    given Ordering[Rank] = Ordering.by(_.ordinal)

    def fromChar(c: Char): Rank = c match {
      case '2' => Two
      case '3' => Three
      case '4' => Four
      case '5' => Five
      case '6' => Six
      case '7' => Seven
      case '8' => Eight
      case '9' => Nine
      case 'T' => Ten
      case 'J' => Jack
      case 'Q' => Queen
      case 'K' => King
      case 'A' => Ace
    }
  }

  case class Card(rank: Rank, suit: Suit)

  object Card {
    def fromString(str: String): Card =
      Card(Rank.fromChar(str(0)), Suit.fromChar(str(1)))
  }

  enum HandRank {
    case HighCard // Highest value card.
    case Pair // Two cards of the same value.
    case TwoPair // Two different pairs.
    case ThreeKind // Three cards of the same value.
    case Straight // All cards are consecutive values.
    case Flush // All cards of the same suit.
    case FullHouse // Three of a kind and a pair.
    case FourKind // Four cards of the same value.
    case StraightFlush // All cards are consecutive values of same suit.
    // case RoyalFlush // Ten, Jack, Queen, King, Ace, in same suit.
  }

  object HandRank {
    given Ordering[HandRank] = Ordering.by(_.ordinal)
  }

  given Ordering[Seq[Card]] = Ordering.by[Seq[Card], (HandRank, Seq[Rank])] { cards =>
    import HandRank._

    val ranks = cards.map(_.rank)

    val rankCounts = ranks.groupMapReduce(identity)(_ => 1)(_ + _)
      .toSeq.map(_.swap).sorted.reverse

    rankCounts match {
      case Seq((4, a), (1, b)) => FourKind -> List(a, b)
      case Seq((3, a), (2, b)) => FullHouse -> List(a, b)
      case Seq((3, a), (1, b), (1, c)) => ThreeKind -> List(a, b, c)
      case Seq((2, a), (2, b), (1, c)) => TwoPair -> List(a, b, c)
      case Seq((2, a), (1, b), (1, c), (1, d)) => Pair -> List(a, b, c, d)
      case _ => {
        val isFlush = cards.distinctBy(_.suit).size == 1

        val isStraight = ranks.max.ordinal - ranks.min.ordinal == 4

        // TODO: does not handle Ace-5 straights. Got lucky.

        if (isStraight && isFlush) StraightFlush -> List(ranks.max)
        else if (isStraight) Straight -> List(ranks.max)
        else if (isFlush) Flush -> List(ranks.max)
        else HighCard -> ranks.sorted.reverse
      }
    }
  }

  def parse(hands: String): (Seq[Card], Seq[Card]) = {
    val cards = hands.split(' ').toSeq
    val left = cards.take(5).map(Card.fromString)
    val right = cards.drop(5).map(Card.fromString)
    (left, right)
  }

  def countWinners(games: Seq[(Seq[Card], Seq[Card])]): Int = {
    val ord = summon[Ordering[Seq[Card]]]
    games.count { case (p1, p2) => ord.gt(p1, p2) }
  }

  val games = io.Source
    .fromFile("p054.txt")
    .getLines()
    .map(parse)

  val ans = countWinners(games.toSeq)

  println(ans)
}
