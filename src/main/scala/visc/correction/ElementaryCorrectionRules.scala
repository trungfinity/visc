package visc.correction

import scala.collection.mutable

import visc.correction.rule.CorrectionRule._
import visc.correction.rule.CorrectionRuleType._
import visc.correction.rule.{CorrectionRule, CorrectionRuleType, RuleValidator}
import visc.util.LinguisticsUtils

object ElementaryCorrectionRules {

  private def expandSyllableParts(
    params: ((String, String), CorrectionRuleType)*
  ): List[(CorrectionRule, CorrectionRuleType)] = {
    params.toList.flatMap {
      case ((partText, correctionText), correctionType) =>
        SyllablePart
          .expand(partText, correctionText)
          .map(_ -> correctionType)
    }
  }

  private val elementaryRules = mutable.Set.empty[(CorrectionRule, CorrectionRuleType)]

  private val expandableChars = LinguisticsUtils.ToneExpansions.keySet ++
    LinguisticsUtils.AccentExpansions.keySet

  expandableChars.foreach { original =>
    LinguisticsUtils
      .expand(original, isAccentExpanded = true)
      .foreach { expansion =>
        val part = if (original == 'd') "^d" else original.toString
        val rule = SyllablePart(part, expansion.toString)
        elementaryRules += rule -> Accent
      }
  }

  elementaryRules ++= expandSyllableParts(
    ("uo", "ư[ơ~]") -> Accent
  )

  elementaryRules ++= List(
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

    Word("ad", "admin") -> ShortForm,

    Word("k", "không") -> ShortForm,
    Word("ko", "không") -> ShortForm,
    Word("hok", "không") -> TeenCode,

    Word("dc", "được") -> ShortForm,
    Word("đc", "được") -> ShortForm,

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
    SyllablePart("x$", "c") -> TeenCode,

    SyllablePart("^k", "c") -> TeenCode,

    Word("j", "gì") -> TeenCode,
    Word("mk", "mình") -> TeenCode
  )

  elementaryRules ++= expandSyllableParts(
    ("^[jz]", "gi") -> TeenCode,
    ("^[jz][i~]", "g[i~]") -> TeenCode,
    ("^z", "v") -> TeenCode,

    ("^ng[i~]", "ngh[i~]") -> TeenCode,
    ("^ng[e~]", "ngh[e~]") -> TeenCode,
    ("^g[e~]", "gh[e~]") -> TeenCode,

    ("j", "[i~]") -> TeenCode,
    ("[i~][cmnptu]$", "i[ê~][cmnptu]") -> TeenCode,
    ("[i~]ng$", "i[ê~]ng") -> TeenCode,
    ("[i~]$", "[a~]y") -> TeenCode,

    ("[ôu~~]$", "[ơ~]") -> TeenCode
  )

  val Elementary = elementaryRules
    .filter {
      case (rule, _) =>
        RuleValidator.validate(rule)
    }
    .toSet
}
