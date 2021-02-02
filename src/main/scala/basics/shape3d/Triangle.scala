package basics.shape3d

import basics.Calculations.average
import basics.shape3d.Abstractions.{Point, Shape}

final case class Triangle private[shape3d](a: Point, b: Point, c: Point, d: Point) extends Shape{
  private def corners = List(a, b, c, d)

  override def x: BigDecimal = average(corners.map(_.x))
  override def y: BigDecimal = average(corners.map(_.y))
  override def z: BigDecimal = average(corners.map(_.z))
  override def minX: BigDecimal = corners.map(_.x).min
  override def maxX: BigDecimal = corners.map(_.x).max
  override def minY: BigDecimal = corners.map(_.y).min
  override def maxY: BigDecimal = corners.map(_.y).max
  override def minZ: BigDecimal = corners.map(_.z).min
  override def maxZ: BigDecimal = corners.map(_.z).max
  override def volume: BigDecimal = ???
  override def surfaceArea: BigDecimal = ???
}
