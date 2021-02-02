package basics.shape3d

object Abstractions {

  trait Shape extends Located with Bounded with SurfaceArea with Volume

  sealed trait Located {
    def x: BigDecimal
    def y: BigDecimal
    def z: BigDecimal
  }

  sealed trait Bounded {
    def minX: BigDecimal
    def maxX: BigDecimal
    def minY: BigDecimal
    def maxY: BigDecimal
    def minZ: BigDecimal
    def maxZ: BigDecimal
  }

  sealed trait SurfaceArea {
    def surfaceArea: BigDecimal
  }

  sealed trait Volume {
    def volume: BigDecimal
  }

  final case class Point(x: BigDecimal, y: BigDecimal, z: BigDecimal)

  object Origin extends Located {
    override def x: BigDecimal = 0
    override def y: BigDecimal = 0
    override def z: BigDecimal = 0
  }

  def cube(center: Point, side: BigDecimal): Cube = Cube(center, side)

  def cuboid(center: Point, width: BigDecimal, height: BigDecimal, length: BigDecimal) : Cuboid =
    Cuboid(center, width, height, length)

  def triangle(a: Point, b: Point, c: Point, d: Point): Triangle = Triangle(a, b, c, d)

  def sphere(center: Point, radius: BigDecimal): Sphere = Sphere(center, radius)

}
