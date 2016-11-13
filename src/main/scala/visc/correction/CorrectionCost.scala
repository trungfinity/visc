package visc.correction

import visc.correction.CorrectionType._
import visc.util.LinguisticsUtils

object CorrectionCost {

  def cost(correctionType: CorrectionType): Double = {
    correctionType match {
      case Insertion => 1
      case Deletion => 1
      case Replacement => 1
      case Transposition => 1

      case FullWord => .75
      case Pronunciation => .35
      case Accent => .35
      case ShortForm => .25
      case TeenCode => .35

      case Penalty => 3
    }
  }

  def cost(correctionTypes: List[CorrectionType]): Double = {
    correctionTypes.foldLeft(0d) { (totalCost, correctionType) =>
      totalCost + cost(correctionType)
    }
  }

  private def flattenValues[K, V](map: Map[K, List[V]]): Set[V] = {
    map
      .values
      .flatten
      .toSet
  }

  private val irreplaceableChars = flattenValues(LinguisticsUtils.ToneExpansions) ++
    flattenValues(LinguisticsUtils.AccentExpansions)

  def cost(
    word: String,
    correction: String
  ): Double = {
    val minCost = Array.ofDim[Double](word.length + 1, correction.length + 1)

    minCost(0)(0) = 0

    for (i <- 1 to word.length) {
      val wordChar = word.charAt(i - 1)
      val deletionCost = if (irreplaceableChars.contains(wordChar)) {
        cost(Penalty)
      } else {
        cost(Deletion)
      }

      minCost(i)(0) = minCost(i - 1)(0) + deletionCost
    }

    for (i <- 1 to correction.length) {
      minCost(0)(i) = i * cost(Insertion)
    }

    for {
      i <- 1 to word.length
      j <- 1 to correction.length
    } {
      val wordChar = word.charAt(i - 1)
      val correctionChar = correction.charAt(j - 1)

      val deletionCost = if (irreplaceableChars.contains(wordChar)) {
        cost(Penalty)
      } else {
        cost(Deletion)
      }

      minCost(i)(j) = Math.min(
        minCost(i - 1)(j) + deletionCost,
        minCost(i)(j - 1) + cost(Insertion)
      )

      for (
        (rule, cost) <- CorrectionRule.Basics
        if rule.matchTransform(word, i, correction, j)
      ) {
        val (wordBeginIndex, correctionBeginIndex) = rule.beginIndices(i, j)

        minCost(i)(j) = Math.min(
          minCost(i)(j),
          minCost(wordBeginIndex)(correctionBeginIndex) + cost
        )
      }

      val replacementCost = if (wordChar != correctionChar) {
        // TODO: Handle penalty
        cost(Replacement)
      } else {
        0
      }

      minCost(i)(j) = Math.min(minCost(i)(j), minCost(i - 1)(j - 1) + replacementCost)

      if (i >= 2 && j >= 2) {
        val prevWordChar = word.charAt(i - 2)
        val prevCorrectionChar = correction.charAt(j - 2)

        if (wordChar == prevCorrectionChar && prevWordChar == correctionChar) {
          minCost(i)(j) = Math.min(minCost(i)(j), minCost(i - 2)(j - 2) + cost(Transposition))
        }
      }
    }

    minCost(word.length)(correction.length)
  }
}
