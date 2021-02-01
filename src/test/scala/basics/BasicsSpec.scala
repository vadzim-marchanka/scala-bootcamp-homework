package basics

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.math.BigInt

class BasicsSpec extends AnyWordSpec with Matchers with ScalaCheckDrivenPropertyChecks {

  private def gcd(a: Int, b: Int): BigInt = BigInt(a).gcd(b)

  implicit val config : PropertyCheckConfiguration = PropertyCheckConfiguration(minSuccessful = 10000)

  "gcd" should {

    "work for non zero values" in {
      forAll { (a: Int, b: Int) =>
        whenever(a != 0 && b != 0) {
          BigInt(Basics.gcd(a, b).get) must equal(gcd(a, b))
        }
      }
    }

    "work if one of the values is zero" in {
      forAll { a: Int =>
        whenever(a != 0) {
          BigInt(Basics.gcd(a, 0).get) must equal(gcd(a, 0))
          BigInt(Basics.gcd(0, a).get) must equal(gcd(0, a))
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
          BigInt(Basics.lcm(a, b).get) must equal((BigInt(a) * BigInt(b) / gcd(a, b)).abs)
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
