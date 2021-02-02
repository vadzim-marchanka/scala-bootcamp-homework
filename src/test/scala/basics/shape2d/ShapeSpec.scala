package basics.shape2d

import basics.shape2d.Abstractions.Point
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ShapeSpec extends AnyWordSpec with Matchers{

  "Square" should  {

    "calculate its area" in {
      val square = Abstractions.square(Point(0, 0), 10)
      square.area should equal(100)
    }

  }

  "Triangle" should {

    "calculate its area" in {
      val triangle = Abstractions.triangle(Point(0, 0), Point(10, 0), Point(0, 5))
      triangle.area should equal(25)
    }

  }

}
