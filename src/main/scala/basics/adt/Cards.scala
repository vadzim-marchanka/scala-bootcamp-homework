package basics.adt

import basics.adt.CardsRestrictions._
import basics.adt.CardsValidations._

trait Cards {

  protected[adt] def values: List[Card]

  protected[adt] def maxGroupWithSameSuite: Cards = maxGroupWithCriteria(_.suit)

  protected[adt] def maxGroupWithSameRank: Cards = maxGroupWithCriteria(_.rank)

  private def maxGroupWithCriteria[T](criteria: Card => T): Cards = {
    val maxGroup = values
      .groupBy(criteria)
      .maxBy { case (_, cards) => cards.size }
      ._2
    Cards0.create(maxGroup)
  }

  protected[adt] def disjoin(cards: Cards): Cards = Cards0.create(values.filterNot(cards.values.contains(_)))

  protected[adt] def first: Option[Card] = values.headOption

}


sealed abstract case class Cards0 private (override val values: List[Card]) extends Cards

object Cards0 {
  def create(cards: List[Card]): Cards = new Cards0(cards.distinct) {}
}


sealed abstract case class Cards1[X1 <: Restriction] private (override val values: List[Card]) extends Cards

object Cards1 {

  def create[X1 <: Restriction: Validation](cards: Cards): Option[Cards1[X1]] =
    Option.when(implicitly[Validation[X1]].isValid(cards))(new Cards1[X1](cards.values){})

}


sealed abstract case class Cards2[
  X1 <: Restriction,
  X2 <: Restriction
] private (override val values: List[Card]) extends Cards

object Cards2 {

  def create[
    X1 <: Restriction: Validation,
    X2 <: Restriction: Validation
  ](cards: Cards): Option[Cards2[X1, X2]] =
    Option.when(
      implicitly[Validation[X1]].isValid(cards) &&
        implicitly[Validation[X2]].isValid(cards)
    )(new Cards2[X1, X2](cards.values){})

}


sealed abstract case class Cards3[
  X1 <: Restriction,
  X2 <: Restriction,
  X3 <: Restriction
] private (override val values: List[Card]) extends Cards

object Cards3 {

  def create[
    X1 <: Restriction: Validation,
    X2 <: Restriction: Validation,
    X3 <: Restriction: Validation
  ](cards: Cards): Option[Cards3[X1, X2, X3]] = Option.when(
      implicitly[Validation[X1]].isValid(cards) &&
        implicitly[Validation[X2]].isValid(cards) &&
        implicitly[Validation[X3]].isValid(cards)
    )(new Cards3[X1, X2, X3](cards.values){})

}


sealed abstract case class Cards4[
  X1 <: Restriction, X2 <: Restriction,
  X3 <: Restriction, X4 <: Restriction
] private (override val values: List[Card]) extends Cards

object Cards4 {

  def create[
    X1 <: Restriction: Validation, X2 <: Restriction: Validation,
    X3 <: Restriction: Validation, X4 <: Restriction: Validation
  ](cards: Cards): Option[Cards4[X1, X2, X3, X4]] =
    Option.when(
      implicitly[Validation[X1]].isValid(cards) && implicitly[Validation[X2]].isValid(cards) &&
        implicitly[Validation[X3]].isValid(cards) && implicitly[Validation[X4]].isValid(cards)
    )(new Cards4[X1, X2, X3, X4](cards.values){})

}
