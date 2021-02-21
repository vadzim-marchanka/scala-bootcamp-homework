package basics

import basics.Operations.{Divide, Min, Sum, Max, Average}
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class OperationsSpec extends AnyWordSpec with Matchers with EitherValues {

  "Divide" should {

    "return errors if the second number is zero" in {
      Divide.calculate(Seq(BigDecimal(1), BigDecimal(0))).left.value should not be empty
    }

    "return errors if list is empty" in {
      Divide.calculate(Seq.empty).left.value should not be empty
    }

    "return errors if there is one number" in {
      Divide.calculate(Seq(BigDecimal(1))).left.value should not be empty
    }

    "return errors if there are more than two numbers" in {
      Divide.calculate(Seq(BigDecimal(1), BigDecimal(0), BigDecimal(1), BigDecimal(0))).left.value should not be empty
    }

    "calculate result" in {
      Divide.calculate(Seq(BigDecimal(6), BigDecimal(3))).right.value should equal(BigDecimal(2))
    }

  }

  "Sum" should {

    "return errors if list is empty" in {
      Sum.calculate(Seq.empty).left.value should not be empty
    }

    "calculate result" in {
      Sum.calculate(Seq(BigDecimal(6), BigDecimal(3), BigDecimal(2))).right.value should equal(BigDecimal(11))
    }

  }

  "Min" should {

    "return errors if list is empty" in {
      Min.calculate(Seq.empty).left.value should not be empty
    }

    "calculate result" in {
      Min.calculate(Seq(BigDecimal(6), BigDecimal(3), BigDecimal(2))).right.value should equal(BigDecimal(2))
    }

  }

  "Max" should {

    "return errors if list is empty" in {
      Max.calculate(Seq.empty).left.value should not be empty
    }

    "calculate result" in {
      Max.calculate(Seq(BigDecimal(6), BigDecimal(3), BigDecimal(2))).right.value should equal(BigDecimal(6))
    }

  }

  "Average" should {

    "return errors if list is empty" in {
      Average.calculate(Seq.empty).left.value should not be empty
    }

    "calculate result" in {
      Average.calculate(Seq(BigDecimal(6), BigDecimal(4), BigDecimal(2))).right.value should equal(BigDecimal(4))
    }

  }

}
