package basics

import scala.annotation.tailrec

object Basics {

  @tailrec
  def gcd(a: Int, b: Int): Option[Long] = (a, b) match {
    case (0, 0) => None
    case (_, 0) => Some(a.toLong.abs)
    case _ => gcd(b, a % b)
  }

  def lcm(a: Int, b: Int): Option[Long] =
    if (a != 0 && b != 0) gcd(a, b).map(g =>  Math.abs(a.toLong * b / g) ) else None

}
