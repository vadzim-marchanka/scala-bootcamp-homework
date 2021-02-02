package basics.shape2d

object Abstractions {

  trait Shape extends Located with Bounded with Area

  sealed trait Located {
    def x: BigDecimal
    def y: BigDecimal
  }

  sealed trait Bounded {
    def minX: BigDecimal
    def maxX: BigDecimal
    def minY: BigDecimal
    def maxY: BigDecimal
  }

  sealed trait Area {
    def area: BigDecimal
  }

  final case class Point(x: BigDecimal, y: BigDecimal)

  def square(center: Point, side: BigDecimal): Shape = Square(center, side)

  def triangle(a: Point, b: Point, c: Point): Shape = Triangle(a, b, c)

}
