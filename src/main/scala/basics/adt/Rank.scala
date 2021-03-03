package basics.adt

sealed trait Rank {
  def order: Int
}

object Rank {

  object Two extends Rank {
    override val order = 1
  }
  object Three extends Rank {
    override val order = 2
  }
  object Four extends Rank {
    override val order = 3
  }
  object Five extends Rank {
    override val order = 4
  }
  object Six extends Rank {
    override val order = 5
  }
  object Seven extends Rank {
    override val order = 6
  }
  object Eight extends Rank {
    override val order = 7
  }
  object Nine extends Rank {
    override val order = 8
  }
  object Ten extends Rank {
    override val order = 9
  }
  object Jack extends Rank {
    override val order = 10
  }
  object Queen extends Rank {
    override val order = 11
  }
  object King extends Rank {
    override val order = 12
  }
  object Ace extends Rank {
    override val order = 13
  }

  implicit val ordering: Ordering[Rank] = (x, y) => x.order compare y.order

}


