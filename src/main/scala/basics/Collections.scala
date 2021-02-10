package basics

import scala.annotation.tailrec

object Collections extends App {

  /*
     In a sorted list find two numbers which have a gap between
        None for List(1, 2, 3, 4)
        Some((2, 8)) for List(1, 2, 8)
   */
  @tailrec
  def findGap(l: List[Int]): Option[(Int, Int)] = l match {
    case Nil => None
    case _ :: Nil => None
    case first :: second :: tail =>
      if (second - first > 1) Option(first, second) else findGap(second :: tail)
  }

  def min(list: List[Int]): Option[Int] = list.reduceOption(_ min _)

  def scanLeft[T](zero: T)(list: List[T])(f: (T, T) => T): List[T] =
    list match {
      case Nil => List(zero)
      case head :: tail => zero :: scanLeft(f(zero, head))(tail)(f)
    }

  // https://twitter.com/allenholub/status/1357115515672555520/photo/1
  // pass the interview
  def count(s: String): List[(Char, Int)] = {
    s.toSeq.foldLeft[List[(Char, Int)]](List.empty) {
      case (Nil, currentSymbol) => List((currentSymbol, 1))
      case (head :: tail, currentSymbol) => head match {
        case (previousSymbol, occurrence) =>
          if (previousSymbol != currentSymbol) (currentSymbol, 1) :: head :: tail
          else (currentSymbol, occurrence + 1) :: tail
      }
    }.reverse
  }

  // https://leetcode.com/problems/running-sum-of-1d-array/
  def runningSumOf1dArray(nums: Array[Int]): Array[Int] = nums.scan(0)(_ + _).tail

  //https://leetcode.com/problems/shuffle-the-array
  def arrayShuffle(nums: Array[Int], n: Int): Array[Int] =
    nums.take(n).zip(nums.drop(n)).flatMap(p => Array(p._1, p._2))

  // https://leetcode.com/problems/richest-customer-wealth
  def reachestCustomerWealth(accounts: Array[Array[Int]]): Int =
    accounts.foldLeft(0){
      case (answer, account) => answer max account.sum
    }

  // https://leetcode.com/problems/kids-with-the-greatest-number-of-candies/
  def kidsWithTheGreatestNumerOfCandies(candies: Array[Int], extraCandies: Int): Array[Boolean] = {
    val max = candies.max
    candies.map(_ + extraCandies >= max)
  }

  // https://leetcode.com/problems/widest-vertical-area-between-two-points-containing-no-points
  def widestVerticalArea(points: Array[Array[Int]]): Int = {
    val sortedX = points.map(_.head).sorted
    sortedX.foldLeft((0, sortedX.head)){
      case ((answer, previousX), currentX) => (answer max (currentX - previousX), currentX)
    }._1
  }

  // https://leetcode.com/problems/maximum-nesting-depth-of-the-parentheses/
  def maximumNestingDepth(s: String) =
    s.filter(c => c == '(' || c == ')').scanLeft(0){ (acc, current) =>
      if (current == '(') acc + 1 else acc - 1
    }.max

  // https://leetcode.com/problems/split-a-string-in-balanced-strings
  def splitAString(s: String) = {
    s.scanLeft(0){ (acc, current) =>
      if (current == 'R') acc + 1 else acc - 1
    }.count(_ == 0) - 1
  }

  // https://leetcode.com/problems/matrix-block-sum/
  def matrixBlockSum(mat: Array[Array[Int]], K: Int) = {
    val n = mat.length
    val m = mat.head.length
    var answer: Array[Array[Int]] = Array()
    for (i <- mat.indices) {
      var row: Array[Int] = Array()
      for (j <- mat(i).indices) {
        var acc: Int = 0
        for (ki <- (-K to K).map(_ + i) ) {
          for (kj <- (-K to K).map(_ + j) ) {
            if (0 <= ki && ki < n && 0 <= kj && kj < m) {
              acc = acc + mat(ki)(kj)
            }
          }
        }
        row = row :+ acc
      }
      answer = answer :+ row
    }
    answer
  }

}
