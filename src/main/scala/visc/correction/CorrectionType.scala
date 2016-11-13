package visc.correction

sealed abstract class CorrectionType extends Product with Serializable

object CorrectionType {

  // Classic types
  case object Insertion extends CorrectionType
  case object Deletion extends CorrectionType
  case object Replacement extends CorrectionType
  case object Transposition extends CorrectionType

  // Rule-based types
  case object FullWord extends CorrectionType
  case object Accent extends CorrectionType
  case object Pronunciation extends CorrectionType
  case object ShortForm extends CorrectionType
  case object TeenCode extends CorrectionType

  // For preventing some ways of correction
  case object Penalty extends CorrectionType
}
