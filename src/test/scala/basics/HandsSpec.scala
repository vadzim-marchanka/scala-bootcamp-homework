package basics

import basics.adt._
import basics.adt.CardsValidations._
import basics.adt.Rank._
import basics.adt.Suit.{Clubs, Diamonds, Hearts, Spades}
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class HandsSpec extends AnyWordSpec with Matchers {

  private def test(cards: Card*)(toHand: Cards1[CardsRestrictions.Five] => Option[Hand]): Assertion = {
    val fiveCards = Cards1.create[CardsRestrictions.Five](Cards0.create(cards.toList))
    fiveCards.flatMap(toHand).nonEmpty shouldEqual true
    fiveCards.flatMap(Hand.create) shouldEqual fiveCards.flatMap(toHand)
  }

  "Class" should {
    "determine royal flush" in {
      test(Card(Ten, Diamonds), Card(Jack, Diamonds), Card(Queen, Diamonds), Card(King, Diamonds), Card(Ace, Diamonds))(
        RoyalFlush.create
      )
    }

    "determine straight flush" in {
      test(Card(Nine, Diamonds), Card(Ten, Diamonds), Card(Jack, Diamonds), Card(Queen, Diamonds), Card(King, Diamonds))(
        StraightFlush.create
      )
    }

    "determine four of kind" in {
      test(Card(Four, Diamonds), Card(Four, Spades), Card(Four, Hearts), Card(Four, Clubs), Card(Ace, Diamonds))(
        FourOfKind.create
      )
    }

    "determine full house" in {
      test(Card(Four, Diamonds), Card(Four, Clubs), Card(Four, Spades), Card(Five, Hearts), Card(Five, Diamonds))(
        FullHouse.create
      )
    }

    "determine flush" in {
      test(Card(Four, Diamonds), Card(Seven, Diamonds), Card(Eight, Diamonds), Card(Ten, Diamonds), Card(Two, Diamonds))(
        Flush.create
      )
    }

    "determine straight" in {
      test(Card(Seven, Diamonds), Card(Eight, Diamonds), Card(Nine, Diamonds), Card(Ten, Spades), Card(Jack, Diamonds))(
        Straight.create
      )
    }

    "determine three of kind" in {
      test(Card(Four, Diamonds), Card(Four, Spades), Card(Four, Clubs), Card(Ten, Diamonds), Card(Two, Diamonds))(
        ThreeOfKind.create
      )
    }

    "determine two pair" in {
      test(Card(Four, Diamonds), Card(Four, Spades), Card(Three, Clubs), Card(Three, Diamonds), Card(Two, Diamonds))(
        TwoPair.create
      )
    }

    "determine pair" in {
      test(Card(Four, Diamonds), Card(Four, Clubs), Card(Seven, Hearts), Card(Ten, Diamonds), Card(Two, Diamonds))(Pair.create)
    }

    "determine high card" in {
      test(Card(Four, Diamonds), Card(Three, Clubs), Card(Seven, Hearts), Card(Ten, Diamonds), Card(Two, Diamonds))(
        HighCard.create
      )
    }

  }

}
