package basics.adt

case class Card(rank: Rank, suit: Suit)

object Card {

  val rankSuitOrdering: Ordering[Card] = Ordering[(Rank, Suit)].on(c => (c.rank, c.suit))

  implicit val defaultOrdering: Ordering[Card] = rankSuitOrdering.reverse

}


