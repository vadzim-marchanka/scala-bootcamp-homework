package basics

import scala.util.Try

object DataStructures {

  // Exercise. Write a function that checks if all values in a `List` are equal.
  // Think about what you think your function should return if `list` is empty, and why.
  def allEqual[T](list: List[T]): Boolean =
    if (list.isEmpty) true
    else list.foldLeft((true, list.head)) {
      case ((previousCheck, previousElement), currentElement) =>
        (previousCheck && (previousElement == currentElement ), currentElement)
    }._1


  val vegetableWeights = Map(
    ("pumpkins", 10),
    ("cucumbers", 20),
    ("olives", 2),
  )

  val vegetablePrices = Map(
    "tomatoes" -> 4,
    "peppers" -> 5,
    "olives" -> 17,
  )

  val vegetableAmounts = Map(
    "tomatoes" -> 17,
    "peppers" -> 234,
    "olives" -> 32,
    "cucumbers" -> 323,
  )

  // Exercise. Calculate the total cost of all vegetables, taking vegetable amounts (in units) from
  // `vegetableAmounts` and prices per unit from `vegetablePrices`. Assume the price is 10 if not available
  // in `vegetablePrices`.
  val totalVegetableCost: Int = {
    vegetableAmounts.toSeq.map {
      case (vegetable, amount) => amount * vegetablePrices.getOrElse(vegetable, 10)
    }.sum
  }

  // Exercise. Given the vegetable weights (per 1 unit of vegetable) in `vegetableWeights` and vegetable
  // amounts (in units) in `vegetableAmounts`, calculate the total weight per type of vegetable, if known.
  //
  // For example, the total weight of "olives" is 2 * 32 == 64.
  val totalVegetableWeights: Map[String, Int] = { // implement here
    vegetableWeights.collect {
      case (vegetable, amount) if vegetableAmounts.contains(vegetable)  =>
        vegetable -> (vegetableAmounts.getOrElse(vegetable, 0) * amount)
    }
  }

  // Exercise: Return a set with all subsets of the provided set `set` with `n` elements
  // For example, `allSubsetsOfSizeN(Set(1, 2, 3), 2) == Set(Set(1, 2), Set(2, 3), Set(1, 3))`.
  // Hints for implementation:
  //   - Handle the trivial case where `n == 1`.
  //   - For other `n`, for each `set` element `elem`, generate all subsets of size `n - 1` from the set
  //     that don't include `elem`, and add `elem` to them.
  def allSubsetsOfSizeN[A](set: Set[A], n: Int): Set[Set[A]] = if (n == 1) set.map(Set(_))
    else set.flatMap(el => allSubsetsOfSizeN(set - el, n - 1).map(_ + el))

  // Homework
  //
  // Implement a special sort which sorts the keys of a map (K) according to their associated
  // values (V).
  //
  // In case of "ties" (equal values) it should group these keys K into Set-s in the results.
  //
  // The input is a map where keys (K) are the values to be sorted and values are their associated numeric
  // values.
  //
  // The output is a list (in ascending order according to the associated `Int` values) of tuples of `Set`-s
  // with values from K, and the associated value V for these values in the `Set`.
  //
  // For example:
  //
  // Input `Map("a" -> 1, "b" -> 2, "c" -> 4, "d" -> 1, "e" -> 0, "f" -> 2, "g" -> 2)` should result in
  // output `List(Set("e") -> 0, Set("a", "d") -> 1, Set("b", "f", "g") -> 2, Set("c") -> 4)`.
  def sortConsideringEqualValues[T](keysAndValues: Map[T, Int]): List[(Set[T], Int)] =
    keysAndValues.toSeq.groupBy(_._2).map {
      case (value, keysWithValues) => keysWithValues.map(_._1).toSet -> value
    }.toList.sortBy(_._2)
}
