package combination_sum_iii

import scala.collection.mutable.ListBuffer

object Solution extends App {
  def combinationSum3(k: Int, n: Int): List[List[Int]] = {
    val candidates = (1 to 9).toList
    val result = ListBuffer.empty[List[Int]]
    backtrack(result, ListBuffer.empty, candidates, n, 0, k)
    result.toList
    //      .filter(_.size == k)
  }

  private def backtrack(list: ListBuffer[List[Int]], tempList: ListBuffer[Int], candidates: List[Int], remain: Int, start: Int, size: Int): Unit = {
    //    printf("remain: %d, tempList: %s, list: %s, start: %d%n", remain, tempList, list, start)
    if (remain == 0 && !list.contains(tempList) && tempList.size == size) {
      list.addOne(tempList.toList)
    } else if (remain > 0) {
      for (i <- start until candidates.length) {
        val nextRemain = remain - candidates(i)
        tempList.addOne(candidates(i))
        backtrack(list, tempList, candidates, nextRemain, i + 1, size)
        tempList.remove(tempList.size - 1)
      }
    }
  }

  println(combinationSum3(3, 7))
  println(combinationSum3(9, 45))
}
