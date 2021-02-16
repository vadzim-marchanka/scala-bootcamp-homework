package basics.adt

import basics.adt.CardsRestrictions.{AllUniqueRanks, Five, Four, FourUniqueRanks, NotSameRank, NotSameSuite, NotSequential, Restriction, SameRank, SameSuite, Sequential, Three, Two, WithAce, WithoutAce}
import basics.adt.Rank.Ace

object CardsValidations {

  abstract class Validation[T <: Restriction] {
    def isValid(cards: Cards): Boolean
  }

  implicit val twoCardsValidation: Validation[Two] = (cards: Cards) =>  cards.values.size == 2
  implicit val threeCardsValidation: Validation[Three] = (cards: Cards) =>  cards.values.size == 3
  implicit val fourCardsValidation: Validation[Four] = (cards: Cards) =>  cards.values.size == 4
  implicit val fiveCardsValidation: Validation[Five] = (cards: Cards) =>  cards.values.size == 5
  implicit val sameRankValidation: Validation[SameRank] = (cards: Cards) => cards.values.map(_.rank).distinct.size == 1
  implicit val notSameRankValidation: Validation[NotSameRank] = (cards: Cards) => !sameRankValidation.isValid(cards)
  implicit val sameSuiteValidation: Validation[SameSuite] = (cards: Cards) => cards.values.map(_.suit).distinct.size == 1
  implicit val notSameSuiteValidation: Validation[NotSameSuite] = (cards: Cards) => !sameSuiteValidation.isValid(cards)

  //TODO: fix not safe method
  implicit val sequentialValidation: Validation[Sequential] = (cards: Cards) => {
    val sorted = cards.values.sorted
    sorted.head.rank.order - sorted.last.rank.order == sorted.size - 1
  }

  implicit val notSequentialValidation: Validation[NotSequential] = (cards: Cards) =>
    !sequentialValidation.isValid(cards)

  implicit val withAceValidation: Validation[WithAce] = (cards: Cards) => cards.values.exists(_.rank == Ace)
  implicit val withoutAceValidation: Validation[WithoutAce] = (cards: Cards) => !withAceValidation.isValid(cards)

  implicit val hasFourUniqueRanksValidation: Validation[FourUniqueRanks] =
    (cards: Cards) => cards.values.map(_.rank).distinct.size == 4

  implicit val hasAllUniqueValidation: Validation[AllUniqueRanks] =
    (cards: Cards) => cards.values.map(_.rank).distinct.size == cards.values.size

}
