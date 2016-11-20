package visc.correction.rule

import scala.collection.mutable

import visc.correction.SpellingCorrector
import visc.util.{Dictionary, LinguisticsUtils, SyllableUtils}

class RuleBasedCorrector(
  elementaryRules: Set[(CorrectionRule, CorrectionRuleType)],
  cost: CorrectionType => Double,
  maxBasicRuleCost: Double = Double.MaxValue,
  maxExpansionDepth: Int = 2,
  maxExpansionCost: Double = Double.MaxValue,
  inDictionaryRequired: Boolean = true
) extends SpellingCorrector {

  private val elementaryRulesWithCost = elementaryRules
    .map {
      case (rule, ruleType) =>
        rule -> cost(CorrectionType.RuleBased(ruleType))
    }

  private val basicRuleOrdering = Ordering.by(-(_: (CorrectionRule, Double))._2)
  private val basicRuleQueue = mutable.PriorityQueue.empty(basicRuleOrdering)
  private val basicRuleCosts = mutable.Map.empty[CorrectionRule, Double]

  elementaryRulesWithCost.foreach {
    case (elementaryRule, elementaryCost) =>
      if (elementaryCost <= maxBasicRuleCost) {
        basicRuleQueue += elementaryRule -> elementaryCost
        basicRuleCosts += elementaryRule -> elementaryCost
      }
  }

  while (basicRuleQueue.nonEmpty) {
    val (basicRule, basicCost) = basicRuleQueue.dequeue()

    if (basicCost <= basicRuleCosts.getOrElse(basicRule, Double.MaxValue)) {
      for {
        (elementaryRule, elementaryCost) <- elementaryRulesWithCost

        newBasicRule <- elementaryRule.andThen(basicRule).toList ++ basicRule.andThen(elementaryRule)
        if RuleValidator.validate(newBasicRule)

        newBasicCost = basicCost + elementaryCost
        if newBasicCost <= maxBasicRuleCost && newBasicCost < basicRuleCosts.getOrElse(newBasicRule, Double.MaxValue)
      } {
        basicRuleQueue += newBasicRule -> newBasicCost
        basicRuleCosts += newBasicRule -> newBasicCost
      }
    }
  }

  private val basicRules = basicRuleCosts.toArray

  printf("Elementary: %d rules.\n", elementaryRules.size)
  printf("Basic: %d rules.\n", basicRules.length)

  private def ruleLevel(rule: CorrectionRule): Int = {
    rule match {
      case rule: CorrectionRule.SyllablePart =>
        if (rule.prefixRequired) {
          if (rule.suffixRequired) 4 else 1
        } else {
          if (rule.suffixRequired) 3 else 2
        }

      case _: CorrectionRule.Word =>
        5
    }
  }

  private def correctSyllable(
    depth: Int,
    currentSyllable: String,
    currentCost: Double,
    lastRuleLevel: Int,
    lastValidationStatus: Boolean,
    corrections: mutable.Set[String]
  ): Unit = {
    if (depth <= 0 || lastValidationStatus) {
      corrections += currentSyllable
    }

    if (depth < maxExpansionDepth) {
      for {
        (rule, cost) <- basicRules
        currentRuleLevel = ruleLevel(rule)
        if currentRuleLevel >= lastRuleLevel

        newCost = currentCost + cost
        if newCost < maxExpansionCost

        newSyllable <- rule.transform(currentSyllable)
        newValidationStatus = SyllableUtils.validate(newSyllable)
        if newValidationStatus >= lastValidationStatus
      } {
        val newDepth = if (depth == 0 &&
          rule.isInstanceOf[CorrectionRule.SyllablePart] &&
          rule.asInstanceOf[CorrectionRule.SyllablePart].prefixRequired) {
          // Hack to extend the maximum expansion depth a little bit
          depth
        } else {
          depth + 1
        }

        correctSyllable(
          depth = newDepth,
          currentSyllable = newSyllable,
          currentCost = newCost,
          lastRuleLevel = currentRuleLevel,
          lastValidationStatus = newValidationStatus,
          corrections = corrections
        )
      }
    }
  }

  private[correction] def syllableCorrections(syllable: String): Set[String] = {
    val corrections = mutable.Set.empty[String]

    correctSyllable(
      depth = 0,
      currentSyllable = syllable,
      currentCost = 0,
      lastRuleLevel = 0,
      lastValidationStatus = SyllableUtils.validateText(syllable),
      corrections = corrections
    )

    corrections.toSet
  }

  private def filterCorrection(correction: String): Boolean = {
    !inDictionaryRequired || RuleBasedCorrector.words.contains(correction)
  }

  private[correction] def corrections(
    syllables: Array[String],
    syllableCorrectionsList: Array[Set[String]]
  ): Set[String] = {
    if (syllables.length == 1) {
      syllableCorrectionsList(0)
        .filter(filterCorrection)

    } else if (syllables.length == 2) {
      val corrections = mutable.Set.empty[String]

      for {
        firstSyllableCorrection <- syllableCorrectionsList(0)
        if RuleBasedCorrector.prefixes.contains(firstSyllableCorrection)

        secondSyllableCorrection <- syllableCorrectionsList(1)

        correction = firstSyllableCorrection + " " + secondSyllableCorrection
        if filterCorrection(correction)
      } corrections += correction

      corrections.toSet

    } else {
      Set(syllables.mkString(" "))
    }
  }

  def corrections(word: String): Set[String] = {
    val syllables = word.split("\\s+")
    val syllableCorrectionsList = syllables.map(syllableCorrections)
    corrections(syllables, syllableCorrectionsList)
  }

  private def flattenChars(chars: Map[Char, List[Char]]): Set[Char] = {
    chars
      .values
      .flatten
      .toSet
  }

  private val irreplaceableChars = flattenChars(LinguisticsUtils.ToneExpansions) ++
    flattenChars(LinguisticsUtils.AccentExpansions)

  def distance(word: String, correction: String): Double = {
    val minCost = Array.ofDim[Double](word.length + 1, correction.length + 1)

    minCost(0)(0) = 0

    for (i <- 1 to word.length) {
      val wordChar = word.charAt(i - 1)
      val deletionCost = if (irreplaceableChars.contains(wordChar)) {
        cost(CorrectionType.Penalty)
      } else {
        cost(CorrectionType.Deletion)
      }

      minCost(i)(0) = minCost(i - 1)(0) + deletionCost
    }

    for (i <- 1 to correction.length) {
      minCost(0)(i) = i * cost(CorrectionType.Insertion)
    }

    for {
      i <- 1 to word.length
      j <- 1 to correction.length
    } {
      val wordChar = word.charAt(i - 1)
      val correctionChar = correction.charAt(j - 1)

      val deletionCost = if (irreplaceableChars.contains(wordChar)) {
        cost(CorrectionType.Penalty)
      } else {
        cost(CorrectionType.Deletion)
      }

      minCost(i)(j) = Math.min(
        minCost(i - 1)(j) + deletionCost,
        minCost(i)(j - 1) + cost(CorrectionType.Insertion)
      )

      for (
        (rule, cost) <- basicRules
        if rule.matchTransform(word, i, correction, j)
      ) {
        val (wordBeginIndex, correctionBeginIndex) = rule.beginIndices(i, j)

        minCost(i)(j) = Math.min(
          minCost(i)(j),
          minCost(wordBeginIndex)(correctionBeginIndex) + cost
        )
      }

      val replacementCost = if (wordChar != correctionChar) {
        // TODO: Handle replacement of adjacent keys on keyboard
        cost(CorrectionType.Penalty) // Prevent replacement
      } else {
        0
      }

      minCost(i)(j) = Math.min(minCost(i)(j), minCost(i - 1)(j - 1) + replacementCost)

      if (i >= 2 && j >= 2) {
        val prevWordChar = word.charAt(i - 2)
        val prevCorrectionChar = correction.charAt(j - 2)

        if (wordChar == prevCorrectionChar && prevWordChar == correctionChar) {
          minCost(i)(j) = Math.min(minCost(i)(j), minCost(i - 2)(j - 2) + cost(CorrectionType.Transposition))
        }
      }
    }

    minCost(word.length)(correction.length)
  }
}

object RuleBasedCorrector {

  private val words = Dictionary.Words.toSet
  private val prefixes = words.flatMap { word =>
    val syllables = word.split("\\s+")

    if (syllables.length > 1) {
      Some(syllables(0))
    } else {
      None
    }
  }
}
