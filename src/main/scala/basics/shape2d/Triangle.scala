package basics.shape2d

import Abstractions.{Point, Shape}
import basics.Calculations.average

final case class Triangle private[shape2d](a: Point, b: Point, c: Point) extends Shape {
  private def corners: List[Point] = List(a, b, c)

  override def minX: BigDecimal = corners.map(_.x).min
  override def maxX: BigDecimal = corners.map(_.x).max
  override def minY: BigDecimal = corners.map(_.y).min
  override def maxY: BigDecimal = corners.map(_.y).max
  override def area: BigDecimal = ((b.x - a.x)*(c.y - a.y) - (c.x - a.x)*(b.y - a.y)).abs / 2
  override def x: BigDecimal = average(corners.map(_.x))
  override def y: BigDecimal = average(corners.map(_.y))
}
