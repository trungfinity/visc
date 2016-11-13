package visc.correction

import scala.collection.mutable

import visc.util.{EditUtils, LinguisticsUtils, SyllableUtils}

object Correction {

  val MaxExpansionDepth = 3
  val MaxExpansionCost = Double.MaxValue

  val MaxEditDistance = 3

  val MaxCost = 2

  private def expandSyllable(
    depth: Int,
    syllable: String,
    cost: Double,
    expansionBuilder: mutable.Builder[String, Set[String]]
  ): Unit = {
    if (depth <= 0 || SyllableUtils.validateText(syllable)) {
      expansionBuilder += syllable
    }

    if (depth < MaxExpansionDepth) {
      for {
        (rule, elementaryCost) <- CorrectionRule.Elements

        newCost = cost + elementaryCost
        if newCost < MaxExpansionCost

        transformed <- rule.transform(syllable)
      } expandSyllable(depth + 1, transformed, newCost, expansionBuilder)
    }
  }

  private def expandSyllable(syllable: String): Set[String] = {
    val expansionBuilder = Set.newBuilder[String]
    expandSyllable(0, syllable, 0, expansionBuilder)
    expansionBuilder.result()
  }

  private def expand(
    syllables: List[String],
    expandedSyllables: List[String],
    expansionBuilder: mutable.Builder[String, Set[String]]
  ): Unit = {
    syllables match {
      case firstSyllable :: remainingSyllables =>
        expandSyllable(firstSyllable).foreach { expandedSyllable =>
          expand(
            remainingSyllables,
            expandedSyllable :: expandedSyllables,
            expansionBuilder
          )
        }

      case Nil =>
        expansionBuilder += expandedSyllables
          .reverse
          .mkString(" ")
    }
  }

  def expand(word: String, inDictionaryRequired: Boolean = true): Set[String] = {
    val syllables = word.split("\\s+").toList
    val expansionBuilder = Set.newBuilder[String]

    expand(syllables, List.empty, expansionBuilder)

    expansionBuilder
      .result()
      .filter { expansion =>
        !inDictionaryRequired || Dictionary.WordSet.contains(expansion)
      }
  }

  private val dictionary = Dictionary.Words.toArray
  private val suggestions = mutable.Map.empty[String, Any]

  private def addSuggestion(delete: String, dictionaryIndex: Int): Unit = {
    suggestions.get(delete).fold[Any] {
      suggestions += delete -> dictionaryIndex
    } {
      case item: Int =>
        val suggestionDictionaryIndices = List(item, dictionaryIndex)
        suggestions += delete -> suggestionDictionaryIndices

      case item =>
        val suggestionDictionaryIndices = item.asInstanceOf[List[Int]]
        suggestions += delete -> (dictionaryIndex :: suggestionDictionaryIndices)
    }
  }

  for (dictionaryIndex <- dictionary.indices) {
    if ((dictionaryIndex + 1) % 10000 == 0) {
      printf("Preprocessing %d words..\n", dictionaryIndex + 1)
    }

    for (delete <- EditUtils.deletes(dictionary(dictionaryIndex), MaxEditDistance)) {
      addSuggestion(delete, dictionaryIndex)
    }
  }

  printf("Processed %d words..\n", dictionary.length)

  def suggest(word: String): Set[String] = {
    val normalized = LinguisticsUtils.normalize(word)
    val suggestionDictionaryIndexBuilder = Set.newBuilder[Int]

    for (delete <- EditUtils.deletes(normalized, MaxEditDistance)) {
      suggestions.get(delete).foreach {
        case item: Int =>
          suggestionDictionaryIndexBuilder += item

        case item =>
          suggestionDictionaryIndexBuilder ++= item.asInstanceOf[List[Int]]
      }
    }

    suggestionDictionaryIndexBuilder
      .result()
      .flatMap { suggestionDictionaryIndex =>
        val suggestionWord = dictionary(suggestionDictionaryIndex)
        val editDistance = EditUtils.editDistance(suggestionWord, word)

        if (editDistance <= MaxEditDistance) {
          Some(suggestionWord)
        } else {
          None
        }
      }
  }

  def correct(word: String): List[(String, Double)] = {
    (expand(word) ++ suggest(word))
      .toList
      .flatMap { correction =>
        val cost = CorrectionCost.cost(word, correction)

        if (cost <= MaxCost) {
          Some(correction -> cost)
        } else {
          None
        }
      }
      .sortBy {
        case (correction, cost) =>
          cost -> correction
      }
  }

//  def correctSentence(sentence: String): List[(String, Double)] = {
//    val syllables = sentence.split("\\s+").toList
//  }
}
