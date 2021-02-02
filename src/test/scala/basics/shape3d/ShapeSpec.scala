package basics.shape3d

import basics.shape3d.Abstractions.Point
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ShapeSpec extends AnyWordSpec with Matchers {

  "Cube" should  {

    "calculate its volume" in {
      val cube = Abstractions.cube(Point(0, 0, 0), 10)
      cube.volume should equal(1000)
    }

  }

  "Cuboid" should {

    "calculate its volume" in {
      val cuboid = Abstractions.cuboid(Point(0, 0, 0), 10, 20, 30)
      cuboid.volume should equal(6000)
    }

  }

  "Triangle" should {

    "throw exception when calculate its volume" in {
      val triangle = Abstractions.triangle(Point(0, 0, 0), Point(0, 10, 0), Point(0, 5, 0), Point(0, 0, 5))
      an [NotImplementedError] should be thrownBy triangle.volume
    }

  }

  "Sphere" should {

    "calculate its volume" in {
      val triangle = Abstractions.sphere(Point(0, 0, 0), 1)
      triangle.volume should equal(4 / 3 * Math.PI)
    }

  }

}
