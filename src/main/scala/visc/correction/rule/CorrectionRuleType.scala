package visc.correction.rule

sealed abstract class CorrectionRuleType extends Product with Serializable

object CorrectionRuleType {
  case object FullWord extends CorrectionRuleType
  case object Accent extends CorrectionRuleType
  case object Pronunciation extends CorrectionRuleType
  case object ShortForm extends CorrectionRuleType
  case object TeenCode extends CorrectionRuleType
}
