package basics.shape3d

import basics.Calculations.{maxCoordinate, minCoordinate}
import basics.shape3d.Abstractions.{Point, Shape}

final case class Cube private[shape3d](center: Point, side: BigDecimal) extends Shape {
  override def x: BigDecimal = center.x
  override def y: BigDecimal = center.y
  override def z: BigDecimal = center.z
  override def minX: BigDecimal = minCoordinate(center.x, side)
  override def maxX: BigDecimal = maxCoordinate(center.x, side)
  override def minY: BigDecimal = minCoordinate(center.y, side)
  override def maxY: BigDecimal = maxCoordinate(center.y, side)
  override def minZ: BigDecimal = minCoordinate(center.z, side)
  override def maxZ: BigDecimal = maxCoordinate(center.z, side)
  override def volume: BigDecimal = side.pow(3)
  override def surfaceArea: BigDecimal = 6 * side * side
}
