package basics.shape2d

import Abstractions.{Point, Shape}
import basics.Calculations.{maxCoordinate, minCoordinate}

final case class Square private[shape2d](center: Point, side: BigDecimal) extends Shape {
  override def x: BigDecimal = center.x
  override def y: BigDecimal = center.y
  override def minX: BigDecimal = minCoordinate(center.x, side)
  override def maxX: BigDecimal = maxCoordinate(center.x, side)
  override def minY: BigDecimal = minCoordinate(center.y, side)
  override def maxY: BigDecimal = maxCoordinate(center.y, side)
  override def area: BigDecimal = side * side
}
