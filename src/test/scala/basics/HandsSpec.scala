package basics

import basics.adt.{Card, Cards1, CardsRestrictions}
import basics.adt.Cards.create
import basics.adt.CardsValidations._
import basics.adt.Hands.Hand
import basics.adt.Hands._
import basics.adt.Rank._
import basics.adt.Suit.{Clubs, Diamonds, Hearts, Spades}
import org.scalatest.Assertion
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class HandsSpec extends AnyWordSpec with Matchers {
  
  private def test(cards: Card*)(toHand : Cards1[CardsRestrictions.Five] => Option[Hand]): Assertion = {
    val fiveCards = create[CardsRestrictions.Five](create(cards.toList))
    fiveCards.flatMap(toHand).nonEmpty shouldEqual true
    fiveCards.flatMap(hand) shouldEqual fiveCards.flatMap(toHand)
  }

  "Class" should {
    "determine royal flush" in {
        test(Card(Ten, Diamonds), Card(Jack, Diamonds), Card(Queen, Diamonds), Card(King, Diamonds), Card(Ace, Diamonds))(royalFlush)
    }

    "determine straight flush" in {
      test(Card(Nine, Diamonds), Card(Ten, Diamonds), Card(Jack, Diamonds), Card(Queen, Diamonds), Card(King, Diamonds))(straightFlush)
    }

    "determine four of kind" in {
      test(Card(Four, Diamonds), Card(Four, Spades), Card(Four, Hearts), Card(Four, Clubs), Card(Ace, Diamonds))(fourOfKind)
    }

    "determine full house" in {
      test(Card(Four, Diamonds), Card(Four, Clubs), Card(Four, Spades), Card(Five, Hearts), Card(Five, Diamonds))(fullHouse)
    }

    "determine flush" in {
      test(Card(Four, Diamonds), Card(Seven, Diamonds), Card(Eight, Diamonds), Card(Ten, Diamonds), Card(Two, Diamonds))(flush)
    }

    "determine straight" in {
      test(Card(Seven, Diamonds), Card(Eight, Diamonds), Card(Nine, Diamonds), Card(Ten, Spades), Card(Jack, Diamonds))(straight)
    }

    "determine three of kind" in {
      test(Card(Four, Diamonds), Card(Four, Spades), Card(Four, Clubs), Card(Ten, Diamonds), Card(Two, Diamonds))(threeOfKind)
    }

    "determine two pair" in {
      test(Card(Four, Diamonds), Card(Four, Spades), Card(Three, Clubs), Card(Three, Diamonds), Card(Two, Diamonds))(twoPair)
    }

    "determine pair" in {
      test(Card(Four, Diamonds), Card(Four, Clubs), Card(Seven, Hearts), Card(Ten, Diamonds), Card(Two, Diamonds))(pair)
    }

    "determine high card" in {
      test(Card(Four, Diamonds), Card(Three, Clubs), Card(Seven, Hearts), Card(Ten, Diamonds), Card(Two, Diamonds))(highCard)
    }

  }

}

