package combination_sum_ii

object Solution extends App {
  def combinationSum2(candidates: Array[Int], target: Int): List[List[Int]] = {
    val sortedCandidates = candidates.filter(_ <= target).sorted.toList
//    println(sortedCandidates)
    var combinations = List.empty[List[Int]]

    sortedCandidates.foreach(sc => {
      combinations = combinations ++ combinations.map(ls => {
        val nextCombination = ls :+ sc
        if (nextCombination.sum <= target) {
          nextCombination
        } else {
          Nil
        }
      })
      combinations = combinations.appended(List(sc))
        .distinct
    })
//    println(combinations)

    combinations.filter(combination => combination.sum == target)
  }

  println(combinationSum2(Array(10, 1, 2, 7, 6, 1, 5), 8))
  println(combinationSum2(Array(2, 5, 2, 1, 2), 5))
  println(combinationSum2(Array(
    14, 6, 25, 9, 30, 20, 33, 34, 28, 30, 16, 12, 31, 9, 9, 12, 34,
    16, 25, 32, 8, 7, 30, 12, 33, 20, 21, 29, 24, 17, 27, 34, 11, 17,
    30, 6, 32, 21, 27, 17, 16, 8, 24, 12, 12, 28, 11, 33, 10, 32, 22,
    13, 34, 18, 12
  ), 27))
}
