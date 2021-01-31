package basics

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.math.BigInt

class BasicsSpec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  private def gcd(a: Int, b: Int): Int = BigInt(a).gcd(b).toInt

  private def lcm(a: Int, b: Int): Long = Math.abs((a.toLong * b) / gcd(a, b))

  "gcd" should {

    "work for non zero values" in {
      forAll { (a: Int, b: Int) =>
        whenever(a != 0 && b != 0) {
          Basics.gcd(a, b) must equal(Some(gcd(a, b)))
        }
      }
    }

    "work if one of the values is zero" in {
      forAll { a: Int =>
        whenever(a != 0) {
          Basics.gcd(a, 0) must equal(Some(gcd(a, 0)))
          Basics.gcd(0, a) must equal(Some(gcd(0, a)))
        }
      }
    }

    "return None if two values are zero" in {
      Basics.gcd(0, 0) must equal(None)
    }

  }

  "lcm" should {

    "work for non zero values" in {
      forAll { (a: Int, b: Int) =>
        whenever(a != 0 && b != 0) {
          Basics.lcm(a, b) must equal(Some(lcm(a, b)))
        }
      }
    }

    "return None if one of the values are zero" in {
      forAll { (a: Int) =>
        Basics.lcm(a, 0) must equal(None)
        Basics.lcm(0, a) must equal(None)
      }
    }

  }

}
