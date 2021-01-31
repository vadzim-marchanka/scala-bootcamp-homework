package basics

import scala.annotation.tailrec

object Basics {

  @tailrec
  def gcd(a: Int, b: Int): Option[Int] = (a, b) match {
    case (0, 0) => None
    case (0, v) => Some(v.abs)
    case (v, 0) => Some(v.abs)
    case _ => gcd(b, a % b)
  }

  def lcm(a: Int, b: Int): Option[Long] =
    if (a != 0 && b != 0) gcd(a, b).map(g =>  Math.abs(a.toLong * b / g) ) else None

}
