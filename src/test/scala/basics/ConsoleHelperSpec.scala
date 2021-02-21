package basics

import basics.Operations.{Average, Divide, Max, Min, Sum}
import basics.ConsoleHelper.{composeResult, parse}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ConsoleHelperSpec extends AnyWordSpec with Matchers {

  "ConsoleHelper" should {

    "parse operation name and digits" in {
      parse("sum 1 2") should equal( (Some(Sum), Seq(BigDecimal(1), BigDecimal(2))) )
    }

    "parse operation name" in {
      parse("  sum ") should equal( (Some(Sum), Seq()) )
    }

    "parse operation name case insensively" in {
      parse("  sUm ") should equal( (Some(Sum), Seq()) )
    }

    "choose first operation if multiple are specified" in {
      parse("  sUm  min") should equal( (Some(Sum), Seq()) )
    }

    "choose operation even if there are letters around" in {
      parse("klsjdsumlaskdjf") should equal( (Some(Sum), Seq()) )
    }

    "parse long numbers" in {
      parse("-123972348172943873012983740182734089172308497.293849812943801283409182309481029384 ") should
        equal( (None, Seq(BigDecimal("-123972348172943873012983740182734089172308497.293849812943801283409182309481029384"))) )
    }

    "parse operation and multiple numbers" in {
      parse("ksajdf;ljasd;lfjlskajdfoiwqerSUMalksjdfl;asjdf;lj asf" +
        " 12alsdfjhasdjkfhakjsdfh-123972348172943873012983740182734089172308497" +
        ".293849812943801283409182309481029384") should
        equal( (Some(Sum), Seq(BigDecimal(12), BigDecimal("-123972348172943873012983740182734089172308497.293849812943801283409182309481029384"))) )
    }

    "compose result for divide operation" in {
      composeResult(Divide, Seq(BigDecimal(5), BigDecimal(2)), BigDecimal(2.5)) should
        equal("5 divided by 2 is 2.5")
    }

    "compose result for sum" in {
      composeResult(Sum, Seq(BigDecimal(1.5), BigDecimal(1.0), BigDecimal(2.5), BigDecimal(3.0)), BigDecimal(8)) should
        equal("the sum of 1.5 1.0 2.5 3.0 is 8")
    }

    "compose result for average" in {
      composeResult(Average, Seq(BigDecimal(1.5), BigDecimal(1.0), BigDecimal(2.5), BigDecimal(3.0)), BigDecimal(2)) should
        equal("the average of 1.5 1.0 2.5 3.0 is 2")
    }

    "compose result for min" in {
      composeResult(Min, Seq(BigDecimal(1.5), BigDecimal(-1.5), BigDecimal(2.5)), BigDecimal(-1.5)) should
        equal("the minimum of 1.5 -1.5 2.5 is -1.5")
    }

    "combine result for max" in {
      composeResult(Max, Seq(BigDecimal(1.5), BigDecimal(1.5), BigDecimal(10.5)), BigDecimal(10.5)) should
        equal("the maximum of 1.5 1.5 10.5 is 10.5")
    }

  }

}
