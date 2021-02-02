package basics

private[basics] object Calculations {
  def minCoordinate(center: BigDecimal, side: BigDecimal) = center - side / 2
  def maxCoordinate(center: BigDecimal, side: BigDecimal) = center + side / 2
  def average(elements: List[BigDecimal]) = elements.sum / elements.size
}
