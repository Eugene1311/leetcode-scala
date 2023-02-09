package combination_sum_ii

import scala.collection.mutable.ListBuffer
import util.control.Breaks._

object BackTrackingSolution extends App {
  def combinationSum2(candidates: Array[Int], target: Int): List[List[Int]] = {
    val result = ListBuffer.empty[List[Int]]
    backtrack(result, ListBuffer.empty, candidates.filter(_ <= target).sorted.toList, target, 0)
    result.toList
  }

  private def backtrack(list: ListBuffer[List[Int]], tempList: ListBuffer[Int], candidates: List[Int], remain: Int, start: Int): Unit = {
    printf("remain: %d, tempList: %s, list: %s, start: %d%n", remain, tempList, list, start)
    if (remain == 0 && !list.contains(tempList)) {
      list.addOne(tempList.toList)
    } else if (remain > 0) {
      for (i <- start until candidates.length) {
        breakable {
          if (i > start && (candidates(i) == candidates(i - 1))) break
          val nextRemain = remain - candidates(i)
          if (nextRemain < 0) break
          tempList.addOne(candidates(i))
          backtrack(list, tempList, candidates, nextRemain, i + 1)
          tempList.remove(tempList.size - 1)
        }
      }
    }
  }

//  println(combinationSum2(Array(10, 1, 2, 7, 6, 1, 5), 8))
//  println(combinationSum2(Array(2, 5, 2, 1, 2), 5))
//  println(combinationSum2(Array(
//    14, 6, 25, 9, 30, 20, 33, 34, 28, 30, 16, 12, 31, 9, 9, 12, 34,
//    16, 25, 32, 8, 7, 30, 12, 33, 20, 21, 29, 24, 17, 27, 34, 11, 17,
//    30, 6, 32, 21, 27, 17, 16, 8, 24, 12, 12, 28, 11, 33, 10, 32, 22,
//    13, 34, 18, 12
//  ), 27))
//    println(combinationSum2(Array(
//      1,1,1,1,1,1,1,1,1,1,
//      1,1,1,1,1,1,1,1,1,1,
//      1,1,1,1,1,1,1,1,1,1,
//      1,1,1,1,1,1,1,1,1,1,
//      1,1,1,1,1,1,1,1,1,1,
//      1,1,1,1,1,1,1,1,1,1,
//      1,1,1,1,1,1,1,1,1,1,
//      1,1,1,1,1,1,1,1,1,1,
//      1,1,1,1,1,1,1,1,1,1,
//      1,1,1,1,1,1,1,1,1,1
//    ), 30))

// runs for very loong time
//  println(combinationSum2(Array(
//    1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
//    11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
//    21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
//    31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
//    41, 42, 43, 44, 45, 46, 47, 48, 49, 50,
//    51, 52, 53, 54, 55, 56, 57, 58, 59, 60,
//    61, 62, 63, 64, 65, 66, 67, 68, 69, 70,
//    71, 72, 73, 74, 75, 76, 77, 78, 79, 80,
//    81, 82, 83, 84, 85, 86, 87, 88, 89, 90,
//    91, 92, 93, 94, 95, 96, 97, 98, 99, 100
//  ), 500))

}
