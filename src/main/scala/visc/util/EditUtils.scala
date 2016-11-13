package visc.util

import scala.collection.mutable

object EditUtils {

  def editDistance(first: String, second: String): Int = {
    val minDistance = Array.ofDim[Int](first.length + 1, second.length + 1)

    for (i <- 0 to first.length) minDistance(i)(0) = i
    for (i <- 1 to second.length) minDistance(0)(i) = i

    for {
      i <- 1 to first.length
      j <- 1 to second.length
    } {
      minDistance(i)(j) = Math.min(
        minDistance(i - 1)(j) + 1,
        minDistance(i)(j - 1) + 1
      )

      val replacementCost = if (first.charAt(i - 1) != second.charAt(j - 1)) 1 else 0
      minDistance(i)(j) = Math.min(minDistance(i)(j), minDistance(i - 1)(j - 1) + replacementCost)

      if (i >= 2 && j >= 2 &&
        first.charAt(i - 1) == second.charAt(j - 2) && first.charAt(i - 2) == second.charAt(j - 1)) {
        minDistance(i)(j) = Math.min(minDistance(i)(j), minDistance(i - 2)(j - 2) + 1)
      }
    }

    minDistance(first.length)(second.length)
  }

  def deletes(word: String, maxDeleteDistance: Int): Set[String] = {
    val results = mutable.Set.empty[String]
    deletes(word, maxDeleteDistance, results)
    results.toSet + word
  }

  private def deletes(
    word: String,
    deleteDistance: Int,
    results: mutable.Set[String]
  ): Unit = {
    if (deleteDistance > 0 && word.length > 1) {
      for (i <- word.indices) {
        val delete = word.substring(0, i) + word.substring(i + 1)

        if (!results.contains(delete)) {
          results += delete
          deletes(delete, deleteDistance - 1, results)
        }
      }
    }
  }
}
