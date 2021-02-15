package basics.adt

import basics.adt.CardsRestrictions._

trait Cards {

  protected[adt] def values: List[Card]

  protected[adt] def maxCardsWithSameSuite: Cards = {
    val maxWithSameSuite = values
      .groupBy(_.suit)
      .maxBy { case (_, cards) => cards.size }
      ._2
    Cards.create(maxWithSameSuite)
  }

  protected[adt] def maxCardsWithSameRank: Cards = {
    val maxWithSameRank = values
      .groupBy(_.rank)
      .maxBy { case (_, cards) => cards.size }
      ._2
    Cards.create(maxWithSameRank)
  }

  protected[adt] def disjoin(cards: Cards): Cards = Cards.create(values.filterNot(cards.values.contains(_)))

  protected[adt] def first: Option[Card] = values.headOption

}

case class Cards0 private(override val values: List[Card]) extends Cards
case class Cards1[X1 <: Restriction] private(override val values: List[Card]) extends Cards
case class Cards2[X1 <: Restriction, X2 <: Restriction] private(override val values: List[Card]) extends Cards
case class Cards3[X1 <: Restriction, X2 <: Restriction, X3 <: Restriction] private(override val values: List[Card]) extends Cards
case class Cards4[X1 <: Restriction, X2 <: Restriction, X3 <: Restriction, X4 <: Restriction] private(override val values: List[Card]) extends Cards

object Cards {

  def create(cards: List[Card]): Cards = Cards0(cards.distinct)

  def create[X1 <: Restriction](cards: Cards)(implicit validation1: Validation[X1]): Option[Cards1[X1]] =
    Option.when(validation1.isValid(cards))(Cards1[X1](cards.values))

  def create[X1 <: Restriction, X2 <: Restriction](cards: Cards)(
    implicit validation1: Validation[X1], validation2: Validation[X2]): Option[Cards2[X1, X2]] =
    Option.when(validation1.isValid(cards) && validation2.isValid(cards))(Cards2[X1, X2](cards.values))

  def create[X1 <: Restriction, X2 <: Restriction, X3 <: Restriction](cards: Cards)(
    implicit validation1: Validation[X1], validation2: Validation[X2], validation3: Validation[X3]): Option[Cards3[X1, X2, X3]] =
    Option.when(validation1.isValid(cards) && validation2.isValid(cards) && validation3.isValid(cards))(Cards3[X1, X2, X3](cards.values))

  def create[X1 <: Restriction, X2 <: Restriction, X3 <: Restriction, X4 <: Restriction](cards: Cards)(
    implicit validation1: Validation[X1], validation2: Validation[X2], validation3: Validation[X3], validation4: Validation[X3])
  : Option[Cards4[X1, X2, X3, X4]] = Option.when(validation1.isValid(cards) && validation2.isValid(cards)
      && validation3.isValid(cards) && validation3.isValid(cards))(Cards4[X1, X2, X3, X4](cards.values))


  abstract class Validation[T <: Restriction] {
    def isValid(cards: Cards): Boolean
  }

}
