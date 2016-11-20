package visc.correction.rule

import visc.util.SyllableUtils

object RuleValidator {

  def validate(rule: CorrectionRule): Boolean = {
    rule match {
      case rule: CorrectionRule.Word =>
        SyllableUtils.validate(rule.correction)

      case rule: CorrectionRule.SyllablePart =>
        SyllableUtils.validate(rule.correction, isFullSyllable = false)
    }
  }
}
