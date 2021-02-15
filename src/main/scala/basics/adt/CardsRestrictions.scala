package basics.adt

object CardsRestrictions {

  sealed trait Restriction
  trait Two extends Restriction
  trait Three extends Restriction
  trait Four extends Restriction
  trait Five extends Restriction
  trait SameRank extends Restriction
  trait NotSameRank extends Restriction
  trait SameSuite extends Restriction
  trait NotSameSuite extends Restriction
  trait Sequential extends Restriction
  trait NotSequential extends Restriction
  trait WithAce extends Restriction
  trait WithoutAce extends Restriction
  trait FourUniqueRanks extends Restriction
  trait AllUniqueRanks extends Restriction

}
