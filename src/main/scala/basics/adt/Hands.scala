package basics.adt

import basics.adt.Cards._
import basics.adt.CardsRestrictions._
import basics.adt.CardsValidations._

object Hands {

  sealed trait Hand
  case class RoyalFlush private(cards: Cards4[Five, SameSuite, Sequential, WithAce]) extends Hand
  case class StraightFlush private(cards: Cards4[Five, SameSuite, Sequential, WithoutAce]) extends Hand
  case class FourOfKind private(fourOfKindCards: Cards2[Four, SameRank], card: Card) extends Hand
  case class FullHouse private(threeOfKindCards: Cards2[Three, SameRank], pairCards: Cards2[Two, SameRank]) extends Hand
  case class Flush private(cards: Cards3[Five, SameSuite, NotSequential]) extends Hand
  case class Straight private(cards: Cards3[Five, Sequential, NotSameSuite]) extends Hand
  case class ThreeOfKind private(threeOfKindCards: Cards2[Three, SameRank], remainingCards: Cards1[Two]) extends Hand
  case class TwoPair private(firstPair: Cards2[Two, SameRank], secondPair: Cards2[Two, SameRank], card: Card) extends Hand
  case class Pair private(pair: Cards2[Two, SameRank], remainingCards: Cards1[Three]) extends Hand
  case class HighCard private(cards: Cards4[Five, AllUniqueRanks, NotSameSuite, NotSequential]) extends Hand

  def royalFlush(cards: Cards1[Five]): Option[RoyalFlush] =
    create[Five, SameSuite, Sequential, WithAce](cards).map(RoyalFlush)

  def straightFlush(cards: Cards1[Five]): Option[StraightFlush] =
    create[Five, SameSuite, Sequential, WithoutAce](cards).map(StraightFlush)

  def fourOfKind(cards: Cards1[Five]): Option[FourOfKind] = {
    val maxGroup = cards.maxGroupWithSameRank
    for {
      fourOfKind <- create[Four, SameRank](maxGroup)
      remainingCard <- cards.disjoin(maxGroup).first
    } yield FourOfKind(fourOfKind, remainingCard)
  }

  def fullHouse(cards: Cards1[Five]): Option[FullHouse] = {
    val maxGroup = cards.maxGroupWithSameRank
    for {
      threeOfKind <- create[Three, SameRank](maxGroup)
      pair <- create[Two, SameRank](cards.disjoin(maxGroup))
    } yield FullHouse(threeOfKind, pair)
  }

  def flush(cards: Cards1[Five]): Option[Flush] =
    create[Five, SameSuite, NotSequential](cards).map(Flush)

  def straight(cards: Cards1[Five]): Option[Straight] =
    create[Five, Sequential, NotSameSuite](cards).map(Straight)

  def threeOfKind(cards: Cards1[Five]): Option[ThreeOfKind] = for {
    threeOfKinds <- create[Three, SameRank](cards.maxGroupWithSameRank)
    remainingUniqueCards <- create[Two, AllUniqueRanks](cards.disjoin(threeOfKinds))
    remainingCards <- create[Two](remainingUniqueCards)
  } yield ThreeOfKind(threeOfKinds, remainingCards)

  def twoPair(cards: Cards1[Five]): Option[TwoPair] = for {
    firstPair <- create[Two, SameRank](cards.maxGroupWithSameRank)
    remainingCards = cards.disjoin(firstPair)
    secondPair <- create[Two, SameRank](remainingCards.maxGroupWithSameRank)
    remainingCard <- remainingCards.disjoin(secondPair).first
  } yield TwoPair(firstPair, secondPair, remainingCard)

  def pair(cards: Cards1[Five]): Option[Pair] = for {
    checkedCards <- create[Five, FourUniqueRanks](cards)
    pair <- create[Two, SameRank](checkedCards.maxGroupWithSameRank)
    remainingCards <- create[Three](checkedCards.disjoin(pair))
  } yield Pair(pair, remainingCards)

  def highCard(cards: Cards1[Five]): Option[HighCard] =
    create[Five, AllUniqueRanks, NotSameSuite, NotSequential](cards).map(HighCard)

  def hand(cards: Cards1[Five]): Option[Hand] =
    highCard(cards)
      .orElse(pair(cards))
      .orElse(twoPair(cards))
      .orElse(threeOfKind(cards))
      .orElse(straight(cards))
      .orElse(flush(cards))
      .orElse(fullHouse(cards))
      .orElse(fourOfKind(cards))
      .orElse(straightFlush(cards))
      .orElse(royalFlush(cards))
}
