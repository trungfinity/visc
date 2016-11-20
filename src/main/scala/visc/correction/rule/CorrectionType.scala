package visc.correction.rule

sealed abstract class CorrectionType extends Product with Serializable

object CorrectionType {

  case object Insertion extends CorrectionType
  case object Deletion extends CorrectionType
  case object Replacement extends CorrectionType
  case object Transposition extends CorrectionType

  case class RuleBased(ruleType: CorrectionRuleType) extends CorrectionType

  case object Penalty extends CorrectionType
}
