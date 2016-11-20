package visc.correction.symmetricdelete

import scala.collection.mutable

import visc.correction.SpellingCorrector
import visc.util.{Dictionary, LinguisticsUtils}

class SymmetricDeleteCorrector(maxEditDistance: Int = 2) extends SpellingCorrector {

  private val dictionary = SymmetricDeleteCorrector.dictionary
  private val suggestions = mutable.Map.empty[String, Any]

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

  private def deletes(word: String, maxDeleteDistance: Int): Set[String] = {
    val results = mutable.Set.empty[String]
    deletes(word, maxDeleteDistance, results)
    results.toSet + word
  }

  private def addSuggestion(delete: String, dictionaryIndex: Int): Unit = {
    suggestions.get(delete).fold[Any] {
      suggestions += delete -> dictionaryIndex
    } {
      case item: Int =>
        val suggestionDictionaryIndices = mutable.ListBuffer(item, dictionaryIndex)
        suggestions += delete -> suggestionDictionaryIndices

      case item =>
        val suggestionDictionaryIndices = item.asInstanceOf[mutable.ListBuffer[Int]]
        suggestionDictionaryIndices += dictionaryIndex
    }
  }

  for (dictionaryIndex <- dictionary.indices) {
    if ((dictionaryIndex + 1) % 10000 == 0) {
      printf("Preprocessing %d words..\n", dictionaryIndex + 1)
    }

    for (delete <- deletes(dictionary(dictionaryIndex), maxEditDistance)) {
      addSuggestion(delete, dictionaryIndex)
    }
  }

  printf("Processed %d words..\n", dictionary.length)

  def corrections(word: String): Set[String] = {
    val normalized = LinguisticsUtils.normalize(word)
    val suggestionDictionaryIndices = mutable.Set.empty[Int]

    for (delete <- deletes(normalized, maxEditDistance)) {
      suggestions.get(delete).foreach {
        case item: Int =>
          suggestionDictionaryIndices += item

        case item =>
          suggestionDictionaryIndices ++= item.asInstanceOf[mutable.ListBuffer[Int]]
      }
    }

    suggestionDictionaryIndices
      .flatMap { suggestionDictionaryIndex =>
        val suggestionWord = dictionary(suggestionDictionaryIndex)
        val editDistance = distance(suggestionWord, word)

        if (editDistance <= maxEditDistance) {
          Some(suggestionWord)
        } else {
          None
        }
      }
      .toSet
  }

  def distance(word: String, correction: String): Double = {
    val minDistances = Array.ofDim[Int](word.length + 1, correction.length + 1)

    for (i <- 0 to word.length) minDistances(i)(0) = i
    for (i <- 1 to correction.length) minDistances(0)(i) = i

    for {
      i <- 1 to word.length
      j <- 1 to correction.length
    } {
      minDistances(i)(j) = Math.min(
        minDistances(i - 1)(j) + 1,
        minDistances(i)(j - 1) + 1
      )

      val replacementCost = if (word.charAt(i - 1) != correction.charAt(j - 1)) 1 else 0
      minDistances(i)(j) = Math.min(minDistances(i)(j), minDistances(i - 1)(j - 1) + replacementCost)

      if (i >= 2 && j >= 2 &&
        word.charAt(i - 1) == correction.charAt(j - 2) && word.charAt(i - 2) == correction.charAt(j - 1)) {
        minDistances(i)(j) = Math.min(minDistances(i)(j), minDistances(i - 2)(j - 2) + 1)
      }
    }

    minDistances(word.length)(correction.length)
  }
}

object SymmetricDeleteCorrector {
  private val dictionary = Dictionary.Words.toArray
}
