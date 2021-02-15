package basics.adt

sealed trait Suit {
  def order: Int
}

case object Suit {

  case object Diamonds extends Suit {
    override def order: Int = 0
  }
  case object Clubs extends Suit {
    override def order: Int = 1
  }
  case object Hearts extends Suit {
    override def order: Int = 2
  }
  case object Spades extends Suit {
    override def order: Int = 3
  }

  implicit val ordering: Ordering[Suit] = (x, y) => x.order compare y.order

}
