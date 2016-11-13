package visc.correction

import scala.collection.mutable

import visc.correction.CorrectionRule._
import visc.correction.CorrectionType._
import visc.util.{LinguisticsUtils, SyllableUtils}

private[correction] trait CorrectionRules {

  val MaxBasicCost = Double.MaxValue

  private[this] def expandSyllableParts(
    params: ((String, String), CorrectionType)*
  ): List[(CorrectionRule, CorrectionType)] = {
    params.toList.flatMap {
      case ((partText, correctionText), correctionType) =>
        SyllablePart
          .expand(partText, correctionText)
          .map(_ -> correctionType)
    }
  }

  private[this] def validate(rule: CorrectionRule): Boolean = {
    rule match {
      case rule: Word =>
        SyllableUtils.validate(rule.correction)

      case rule: SyllablePart =>
        SyllableUtils.validate(rule.correction, isFullSyllable = false)
    }
  }

  private[this] val elementaryRuleBuilder = Set.newBuilder[(CorrectionRule, CorrectionType)]

  private[this] val expandableChars = LinguisticsUtils.ToneExpansions.keySet ++
    LinguisticsUtils.AccentExpansions.keySet

  expandableChars.foreach { original =>
    LinguisticsUtils
      .expand(original, isAccentExpanded = true)
      .foreach { expansion =>
        val rule = SyllablePart(original.toString, expansion.toString)
        elementaryRuleBuilder += rule -> Accent
      }
  }

  elementaryRuleBuilder ++= List(
    Word("ih", "đi") -> FullWord,
    Word("hoy", "thôi") -> FullWord,

    SyllablePart("^g", "r") -> Pronunciation,

    SyllablePart("g$", "ng") -> ShortForm,
    SyllablePart("h$", "nh") -> ShortForm,

    Word("a", "anh") -> ShortForm,
    Word("e", "em") -> ShortForm,
    Word("b", "bạn") -> ShortForm,
    Word("m", "mình") -> ShortForm,
    Word("m", "mày") -> ShortForm,
    Word("t", "tao") -> ShortForm,

    Word("k", "không") -> ShortForm,
    Word("ko", "không") -> ShortForm
  )

  elementaryRuleBuilder ++= List(
    SyllablePart("^ck", "ch") -> TeenCode,
    SyllablePart("^gk", "gh") -> TeenCode,
    SyllablePart("^nk", "nh") -> TeenCode,
    SyllablePart("^pk", "ph") -> TeenCode,
    SyllablePart("^tk", "th") -> TeenCode,

    SyllablePart("ck$", "ch") -> TeenCode,
    SyllablePart("nk$", "nh") -> TeenCode,
    SyllablePart("k$", "ch") -> TeenCode,
    SyllablePart("k$", "nh") -> TeenCode,
    SyllablePart("k$", "c") -> TeenCode,

    SyllablePart("^k", "c") -> TeenCode,

    Word("j", "gì") -> TeenCode,
    Word("mk", "mình") -> TeenCode
  )

  elementaryRuleBuilder ++= expandSyllableParts(
    ("^[jz]", "gi") -> TeenCode,
    ("^[jz][i~]", "g[i~]") -> TeenCode,
    ("^z", "v") -> TeenCode,

    ("^ng[i~]", "ngh[i~]") -> TeenCode,
    ("^ng[e~]", "ngh[e~]") -> TeenCode,
    ("^g[e~]", "gh[e~]") -> TeenCode,

    ("j", "[i~]") -> TeenCode,
    ("[i~][cmnptu]$", "i[ê~][cmnptu]") -> TeenCode,
    ("[i~]ng$", "i[ê~]ng") -> TeenCode,

    ("[ôu~~]$", "[ơ~]") -> TeenCode
  )

  val Elements = elementaryRuleBuilder
    .result()
    .filter {
      case (rule, _) =>
        validate(rule)
    }
    .map {
      case (rule, correctionType) =>
        rule -> CorrectionCost.cost(correctionType)
    }
    .toMap

  private[this] val basicRuleOrdering = Ordering.by((_: (CorrectionRule, Double))._2)
  private[this] val basicRules = mutable.PriorityQueue.empty(basicRuleOrdering)
  private[this] val basicRuleCosts = mutable.Map.empty[CorrectionRule, Double]

  Elements.foreach {
    case (rule, cost) =>
      if (cost <= MaxBasicCost) {
        basicRules += rule -> cost
        basicRuleCosts += rule -> cost
      }
  }

  for {
    (rule, cost) <- basicRules
    if cost <= basicRuleCosts.getOrElse(rule, Double.MaxValue)

    (elementaryRule, elementaryCost) <- Elements

    newRule <- elementaryRule.andThen(rule).toList ++ rule.andThen(elementaryRule)
    if validate(newRule)

    newCost = cost + elementaryCost
    if newCost <= MaxBasicCost && newCost < basicRuleCosts.getOrElse(newRule, Double.MaxValue)
  } {
    basicRules += newRule -> newCost
    basicRuleCosts += newRule -> newCost
  }

  val Basics = basicRuleCosts.toMap
}
