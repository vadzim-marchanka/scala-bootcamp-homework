package basics.adt

import basics.adt.CardsRestrictions._
import basics.adt.CardsValidations._

sealed trait Hand


sealed abstract case class RoyalFlush private (cards: Cards4[Five, SameSuite, Sequential, WithAce]) extends Hand

object RoyalFlush {
  def create(cards: Cards1[Five]): Option[RoyalFlush] =
    Cards4.create[Five, SameSuite, Sequential, WithAce](cards).map(new RoyalFlush(_) {})
}


sealed abstract case class StraightFlush private (cards: Cards4[Five, SameSuite, Sequential, WithoutAce]) extends Hand

object StraightFlush {
  def create(cards: Cards1[Five]): Option[StraightFlush] =
    Cards4.create[Five, SameSuite, Sequential, WithoutAce](cards).map(new StraightFlush(_) {})
}


sealed abstract case class FourOfKind private (fourOfKindCards: Cards2[Four, SameRank], card: Card) extends Hand

object FourOfKind {

  def create(cards: Cards1[Five]): Option[FourOfKind] = {
    val maxGroup = cards.maxGroupWithSameRank
    for {
      fourOfKind <- Cards2.create[Four, SameRank](maxGroup)
      remainingCard <- cards.disjoin(maxGroup).first
    } yield new FourOfKind(fourOfKind, remainingCard) {}
  }

}


sealed abstract case class FullHouse private (threeOfKindCards: Cards2[Three, SameRank], pairCards: Cards2[Two, SameRank])
    extends Hand

object FullHouse {

  def create(cards: Cards1[Five]): Option[FullHouse] = {
    val maxGroup = cards.maxGroupWithSameRank
    for {
      threeOfKind <- Cards2.create[Three, SameRank](maxGroup)
      pair <- Cards2.create[Two, SameRank](cards.disjoin(maxGroup))
    } yield new FullHouse(threeOfKind, pair) {}
  }

}


sealed abstract case class Flush private (cards: Cards3[Five, SameSuite, NotSequential]) extends Hand

object Flush {

  def create(cards: Cards1[Five]): Option[Flush] =
    Cards3.create[Five, SameSuite, NotSequential](cards).map(new Flush(_) {})

}


sealed abstract case class Straight private (cards: Cards3[Five, Sequential, NotSameSuite]) extends Hand

object Straight {

  def create(cards: Cards1[Five]): Option[Straight] =
    Cards3.create[Five, Sequential, NotSameSuite](cards).map(new Straight(_) {})

}


sealed abstract case class ThreeOfKind private (threeOfKindCards: Cards2[Three, SameRank], remainingCards: Cards1[Two])
    extends Hand

object ThreeOfKind {

  def create(cards: Cards1[Five]): Option[ThreeOfKind] = for {
    threeOfKinds <- Cards2.create[Three, SameRank](cards.maxGroupWithSameRank)
    remainingUniqueCards <- Cards2.create[Two, AllUniqueRanks](cards.disjoin(threeOfKinds))
    remainingCards <- Cards1.create[Two](remainingUniqueCards)
  } yield new ThreeOfKind(threeOfKinds, remainingCards) {}

}


sealed abstract case class TwoPair private (firstPair: Cards2[Two, SameRank], secondPair: Cards2[Two, SameRank], card: Card)
    extends Hand

object TwoPair {

  def create(cards: Cards1[Five]): Option[TwoPair] = for {
    firstPair <- Cards2.create[Two, SameRank](cards.maxGroupWithSameRank)
    remainingCards = cards.disjoin(firstPair)
    secondPair <- Cards2.create[Two, SameRank](remainingCards.maxGroupWithSameRank)
    remainingCard <- remainingCards.disjoin(secondPair).first
  } yield new TwoPair(firstPair, secondPair, remainingCard) {}

}


sealed abstract case class Pair private (pair: Cards2[Two, SameRank], remainingCards: Cards1[Three]) extends Hand

object Pair {

  def create(cards: Cards1[Five]): Option[Pair] = for {
    checkedCards <- Cards2.create[Five, FourUniqueRanks](cards)
    pair <- Cards2.create[Two, SameRank](checkedCards.maxGroupWithSameRank)
    remainingCards <- Cards1.create[Three](checkedCards.disjoin(pair))
  } yield new Pair(pair, remainingCards) {}

}


sealed case class HighCard private (cards: Cards4[Five, AllUniqueRanks, NotSameSuite, NotSequential]) extends Hand

object HighCard {

  def create(cards: Cards1[Five]): Option[HighCard] =
    Cards4.create[Five, AllUniqueRanks, NotSameSuite, NotSequential](cards).map(new HighCard(_) {})

}


object Hand {

  def create(cards: Cards1[Five]): Option[Hand] =
    HighCard
      .create(cards)
      .orElse(Pair.create(cards))
      .orElse(TwoPair.create(cards))
      .orElse(ThreeOfKind.create(cards))
      .orElse(Straight.create(cards))
      .orElse(Flush.create(cards))
      .orElse(FullHouse.create(cards))
      .orElse(FourOfKind.create(cards))
      .orElse(StraightFlush.create(cards))
      .orElse(RoyalFlush.create(cards))

}
