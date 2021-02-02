package basics.shape3d

import basics.Calculations.{maxCoordinate, minCoordinate}
import basics.shape3d.Abstractions.{Point, Shape}

final case class Cuboid private[shape3d](center: Point, width: BigDecimal, height: BigDecimal, length: BigDecimal)
  extends Shape {
  override def x: BigDecimal = center.x
  override def y: BigDecimal = center.y
  override def z: BigDecimal = center.z
  override def minX: BigDecimal = minCoordinate(center.x, width)
  override def maxX: BigDecimal = maxCoordinate(center.x, width)
  override def minY: BigDecimal = minCoordinate(center.y, height)
  override def maxY: BigDecimal = maxCoordinate(center.y, height)
  override def minZ: BigDecimal = minCoordinate(center.z, length)
  override def maxZ: BigDecimal = maxCoordinate(center.z, length)
  override def surfaceArea: BigDecimal = 2 * width * height + 2 * width * length + 2 * height * length
  override def volume: BigDecimal = width * height * length
}
