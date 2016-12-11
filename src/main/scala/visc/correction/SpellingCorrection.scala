package visc.correction

import scala.collection.mutable

import org.apache.commons.math3.distribution.NormalDistribution

import visc.correction.rule.{CorrectionRuleType, CorrectionType, RuleBasedCorrector}
import visc.correction.symmetricdelete.SymmetricDeleteCorrector
import visc.frequency.Frequency
import visc.util.LinguisticsUtils

class SpellingCorrection {

  println("Loading term frequency..")
  printf("Uni-gram: %d grams.\n", Frequency.UnigramTermFrequency.size)
  printf("Bi-gram: %d grams.\n", Frequency.BigramTermFrequency.size)

  val MaxCost = 2

  private val symmetricDeleteCorrector = new SymmetricDeleteCorrector

  private def correctionCost(correctionType: CorrectionType): Double = {
    correctionType match {
      case CorrectionType.Insertion => 1
      case CorrectionType.Deletion => 1
      case CorrectionType.Replacement => 1
      case CorrectionType.Transposition => 1

      case CorrectionType.RuleBased(CorrectionRuleType.FullWord) => 0.75
      case CorrectionType.RuleBased(CorrectionRuleType.Pronunciation) => 0.35
      case CorrectionType.RuleBased(CorrectionRuleType.Accent) => 0.35
      case CorrectionType.RuleBased(CorrectionRuleType.ShortForm) => 0.25
      case CorrectionType.RuleBased(CorrectionRuleType.TeenCode) => 0.35

      case CorrectionType.Penalty => 3
    }
  }

  private val ruleBasedCorrector = new RuleBasedCorrector(
    elementaryRules = ElementaryCorrectionRules.Elementary,
    cost = correctionCost,
    maxExpansionCost = 1.81
  )

  def correctionCost(correction: String, word: String): Double = {
    val originalCost = ruleBasedCorrector.distance(word, correction)
    val numSyllable = word.count(_ == ' ') + 1
    originalCost / numSyllable
  }

  private def correctionProbability(cost: Double): Double = {
    1 - distribution.cumulativeProbability(cost)
  }

  def correctionProbability(correction: String, word: String): Double = {
    val cost = correctionCost(correction, word)
    correctionProbability(cost)
  }

  def frequencyRatio(word: String): Double = {
    val frequency = Frequency
      .DictionaryIndices
      .get(word)
      .fold(1) { dictionaryIndex =>
        Frequency
          .UnigramTermFrequency
          .getOrElse(dictionaryIndex, 1)
      }

    Math.log(Math.log(frequency) + 1)
  }

  private def correctionScore(correction: String, word: String, probability: Double): Double = {
    val numSyllable = word.count(_ == ' ') + 1
    val syllableBoost = if (numSyllable > 1) 3.5 else 1
    probability * frequencyRatio(correction) * syllableBoost
  }

  def correctionScore(correction: String, word: String): Double = {
    val probability = correctionProbability(correction, word)
    correctionScore(correction, word, probability)
  }

  private val distribution = new NormalDistribution(1.4, 0.6)

  private def wordCorrections(
    word: String,
    ruleBasedCorrections: Set[String]
  ): List[(String, Double)] = {
    (ruleBasedCorrections ++ symmetricDeleteCorrector.corrections(word))
      .toList
      .flatMap { correction =>
        val cost = correctionCost(correction, word)

        if (cost <= MaxCost) {
          Some(correction -> cost)
        } else {
          None
        }
      }
      .flatMap {
        case (correction, cost) =>
          val probability = correctionProbability(cost)

          if ((word.length > 2 && probability >= 0.15) || probability > 0.5) {
            Some(correction -> probability)
          } else {
            None
          }
      }
      .map {
        case (correction, probability) =>
          val score = correctionScore(correction, word, probability)
          correction -> score
      }
      .sortBy {
        case (_, score) =>
          -score
      }
  }

  def wordCorrections(word: String): List[(String, Double)] = {
    wordCorrections(word, ruleBasedCorrector.corrections(word))
  }

  def sentenceCorrection(sentence: String): String = {
    val syllables = sentence.split("\\s+").map(LinguisticsUtils.normalize)
    val syllableCorrectionsList = syllables.map(ruleBasedCorrector.syllableCorrections)

    val maxScores = Array.ofDim[Double](syllables.length + 1)
    val previousStates = Array.ofDim[Int](syllables.length + 1)
    val lastWords = Array.ofDim[String](syllables.length + 1)

    maxScores(0) = 0

    for (i <- 1 to syllables.length) {
      maxScores(i) = maxScores(i - 1)
      previousStates(i) = i - 1
      lastWords(i) = syllables(i - 1)

      val singleSyllableCorrections = wordCorrections(
        word = syllables(i - 1),
        ruleBasedCorrections = ruleBasedCorrector.corrections(
          syllables = Array(syllables(i - 1)),
          syllableCorrectionsList = Array(syllableCorrectionsList(i - 1))
        )
      )

      for ((correction, correctionScore) <- singleSyllableCorrections) {
        val score = maxScores(i - 1) + correctionScore
        // println(s"""f($i) = f(${i - 1}) + score("$correction" | "${syllables(i - 1)}") = $score""")

        if (score > maxScores(i)) {
          maxScores(i) = score
          lastWords(i) = correction
        }
      }

      if (i > 1) {
        val doubleSyllableCorrections = wordCorrections(
          word = syllables(i - 2) + " " + syllables(i - 1),
          ruleBasedCorrections = ruleBasedCorrector.corrections(
            syllables = Array(syllables(i - 2), syllables(i - 1)),
            syllableCorrectionsList = Array(
              syllableCorrectionsList(i - 2),
              syllableCorrectionsList(i - 1)
            )
          )
        )

        for ((correction, correctionScore) <- doubleSyllableCorrections) {
          val score = maxScores(i - 2) + correctionScore
          // println(s"""f($i) = f(${i - 2}) + score("$correction" | "${syllables(i - 2)} ${syllables(i - 1)}") = $score""")

          if (score > maxScores(i)) {
            maxScores(i) = score
            previousStates(i) = i - 2
            lastWords(i) = correction
          }
        }
      }

      // println(s"f[$i] = ${maxScores(i)}")
    }

    var i = syllables.length
    val words = mutable.ListBuffer.empty[String]

    while (i > 0) {
      words += lastWords(i)
      i = previousStates(i)
    }

    val correction = words
      .reverse
      .mkString(" ")

    correction
  }

  private def frequencyRatio(firstWord: String, secondWord: String): Double = {
    val frequencyOpt = for {
      firstDictionaryIndex <- Frequency.DictionaryIndices.get(firstWord)
      secondDictionaryIndex <- Frequency.DictionaryIndices.get(secondWord)
      frequency <- Frequency.BigramTermFrequency.get((firstDictionaryIndex, secondDictionaryIndex))
    } yield frequency.toDouble

    Math.log(Math.log(frequencyOpt.getOrElse(1)) + 1)
  }

  def sentenceCorrection2(sentence: String): String = {
    val syllables = sentence.split("\\s+").map(LinguisticsUtils.normalize)
    val syllableCorrectionsList = syllables.map(ruleBasedCorrector.syllableCorrections)

    val stateOrdering = Ordering.by(-(_: ((Int, String), Double))._2)
    val stateQueue = mutable.PriorityQueue.empty(stateOrdering)
    val stateDistances = mutable.Map.empty[(Int, String), Double]
    val previousCorrections = mutable.Map.empty[(Int, String), String]

    stateQueue += (0, "") -> 0
    stateDistances += (0, "") -> 0

    var lastCorrection: String = null

    while (lastCorrection == null && stateQueue.nonEmpty) {
      val (currentState, currentDistance) = stateQueue.dequeue()
      val (i, currentCorrection) = currentState

      if (i >= syllables.length) {
        lastCorrection = currentCorrection

      } else if (currentDistance <= stateDistances.getOrElse(currentState, Double.MaxValue)) {
        for {
          (nextCorrection, nextCorrectionScore) <- wordCorrections(
            word = syllables(i),
            ruleBasedCorrections = ruleBasedCorrector.corrections(
              syllables = Array(syllables(i)),
              syllableCorrectionsList = Array(syllableCorrectionsList(i))
            )
          ).slice(0, 10)
          nextState = (i + 1, nextCorrection)
          distance = 15 - nextCorrectionScore * frequencyRatio(currentCorrection, nextCorrection)
          nextDistance = currentDistance + distance
          if nextDistance < stateDistances.getOrElse(nextState, Double.MaxValue)
        } {
          stateQueue += nextState -> nextDistance
          stateDistances += nextState -> nextDistance
          previousCorrections += nextState -> currentCorrection
        }

        if (i < syllables.length - 1) {
          for {
            (nextCorrection, nextCorrectionScore) <- wordCorrections(
              word = syllables(i) + " " + syllables(i + 1),
              ruleBasedCorrections = ruleBasedCorrector.corrections(
                syllables = Array(syllables(i), syllables(i + 1)),
                syllableCorrectionsList = Array(
                  syllableCorrectionsList(i),
                  syllableCorrectionsList(i + 1)
                )
              )
            ).slice(0, 10)
            nextState = (i + 2, nextCorrection)
            distance = 30 - nextCorrectionScore * frequencyRatio(currentCorrection, nextCorrection)
            nextDistance = currentDistance + distance
            if nextDistance < stateDistances.getOrElse(nextState, Double.MaxValue)
          } {
            stateQueue += nextState -> nextDistance
            stateDistances += nextState -> nextDistance
            previousCorrections += nextState -> currentCorrection
          }
        }
      }
    }

    var i = syllables.length
    val corrections = mutable.ListBuffer.empty[String]

    while (i > 0) {
      corrections += lastCorrection
      val nearlyLastCorrection = previousCorrections((i, lastCorrection))
      i = i - lastCorrection.count(_ == ' ') - 1
      lastCorrection = nearlyLastCorrection
    }

    corrections
      .reverse
      .mkString(" ")
  }
}
