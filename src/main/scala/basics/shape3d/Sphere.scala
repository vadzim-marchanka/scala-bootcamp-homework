package basics.shape3d

import basics.Calculations.{maxCoordinate, minCoordinate}
import basics.shape3d.Abstractions.{Point, Shape}

final case class Sphere private[shape3d](center: Point, radius: BigDecimal) extends Shape {
  override def x: BigDecimal = center.x
  override def y: BigDecimal = center.y
  override def z: BigDecimal = center.z
  override def minX: BigDecimal = minCoordinate(center.x, radius * 2)
  override def maxX: BigDecimal = maxCoordinate(center.x, radius * 2)
  override def minY: BigDecimal = minCoordinate(center.y, radius * 2)
  override def maxY: BigDecimal = maxCoordinate(center.y, radius * 2)
  override def minZ: BigDecimal = minCoordinate(center.z, radius * 2)
  override def maxZ: BigDecimal = maxCoordinate(center.z, radius * 2)
  override def surfaceArea: BigDecimal = radius * radius * 4 * Math.PI
  override def volume: BigDecimal = 4 / 3 * Math.PI * radius.pow(3)
}
