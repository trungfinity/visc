package visc

sealed abstract class CorrectionRule extends Product with Serializable {
  def andThen(other: CorrectionRule): Option[CorrectionRule]
  def transform(word: String): Option[String]
  def inverseTransform(correction: String): Option[String]
}

object CorrectionRule {

  sealed abstract class Basic extends CorrectionRule {

    def matchWord(word: String, endIndex: Int): Boolean
    def matchCorrection(correction: String, endIndex: Int): Boolean

    def matchTransform(
      word: String,
      wordEndIndex: Int,
      correction: String,
      correctionEndIndex: Int
    ): Boolean = {
      matchWord(word, wordEndIndex) && matchCorrection(correction, correctionEndIndex)
    }

    def beginIndices(wordEndIndex: Int, correctionEndIndex: Int): (Int, Int)
  }

  private def replace(
    word: String,
    beginIndex: Int,
    endIndex: Int,
    replacement: String
  ): String = {
    word.substring(0, beginIndex) + replacement + word.substring(endIndex)
  }

  def zipLongest[T](firstList : List[T], secondList : List[T]): List[(T, T)] = {
    if (firstList.size <= secondList.size) {
      Stream
        .continually(firstList)
        .flatten
        .zip(secondList)
        .toList

    } else {
      zipLongest(secondList, firstList)
    }
  }

  final case class Word(
    word: String,
    correction: String
  ) extends Basic {

    def andThen(other: CorrectionRule): Option[CorrectionRule] = {
      other match {
        case rule: Word =>
          if (correction == rule.word) {
            Some(Word(word, rule.correction))
          } else {
            None
          }

        case _ =>
          None
      }
    }

    private def matchByQuery(query: String, word: String, endIndex: Int): Boolean = {
      endIndex >= word.length && query == word
    }

    def matchWord(word: String, endIndex: Int): Boolean = {
      matchByQuery(this.word, word, endIndex)
    }

    def matchCorrection(correction: String, endIndex: Int): Boolean = {
      matchByQuery(this.correction, correction, endIndex)
    }

    def beginIndices(wordEndIndex: Int, correctionEndIndex: Int): (Int, Int) = {
      (wordEndIndex - word.length, correctionEndIndex - correction.length)
    }

    def transform(word: String): Option[String] = {
      if (word == this.word) Some(correction) else None
    }

    def inverseTransform(correction: String): Option[String] = {
      if (correction == this.correction) Some(word) else None
    }
  }

  object Word {

    def expand(
      shortFormWord: String,
      shortFormCorrection: String
    ): List[Word] = {
      zipLongest(
        StringExpander(shortFormWord),
        StringExpander(shortFormCorrection)
      ).map {
        case (word, correction) =>
          Word(word, correction)
      }
    }
  }

  final case class SyllablePart(
    part: String,
    prefixRequired: Boolean,
    suffixRequired: Boolean,
    correction: String
  ) extends Basic {

    def andThen(other: CorrectionRule): Option[CorrectionRule] = {
      other match {
        case rule: Word =>
          inverseTransform(rule.word)
            .map(Word(_, rule.correction))

        case rule: SyllablePart =>
          if ((prefixRequired && rule.prefixRequired) || (suffixRequired && rule.suffixRequired)) {
            val newPrefixRequired = prefixRequired || rule.prefixRequired
            val newSuffixRequired = suffixRequired || rule.suffixRequired

            rule
              .transform(correction)
              .map { newCorrection =>
                SyllablePart(part, newPrefixRequired, newSuffixRequired, newCorrection)
              }
              .orElse {
                inverseTransform(rule.part)
                  .map { newPart =>
                    SyllablePart(newPart, newPrefixRequired, newSuffixRequired, rule.correction)
                  }
              }

          } else {
            val bothInMiddle = !(prefixRequired || suffixRequired ||
              rule.prefixRequired || rule.suffixRequired)

            if (bothInMiddle && correction == rule.part) {
              Some(SyllablePart(part, rule.correction))

            } else {
              Some(Sequence(List(this, rule)))
            }
          }

        case rule: Sequence =>
          rule.subrules match {
            case firstSubrule :: remainingSubrules =>
              andThen(firstSubrule).flatMap {
                case _: Sequence => Some(Sequence(this :: rule.subrules))
                case newRule => newRule.andThen(Sequence(remainingSubrules))
              }

            case Nil =>
              Some(this)
          }
      }
    }

    private def satisfy(fact: Boolean, required: Boolean): Boolean = {
      !required || fact
    }

    private def matchByQuery(query: String, word: String, endIndex: Int): Boolean = {
      val beginIndex = Math.max(0, endIndex - query.length)
      val isPrefix = beginIndex <= 0 || word.charAt(beginIndex - 1) == ' '
      val isSuffix = endIndex >= word.length || word.charAt(endIndex) == ' '

      satisfy(isPrefix, prefixRequired) &&
        satisfy(isSuffix, suffixRequired) &&
        query == word.substring(beginIndex, endIndex)
    }

    def matchWord(word: String, endIndex: Int): Boolean = {
      matchByQuery(part, word, endIndex)
    }

    def matchCorrection(correction: String, endIndex: Int): Boolean = {
      matchByQuery(this.correction, correction, endIndex)
    }

    def beginIndices(wordEndIndex: Int, correctionEndIndex: Int): (Int, Int) = {
      (wordEndIndex - part.length, correctionEndIndex - correction.length)
    }

    private def transform(
      word: String,
      part: String,
      matchPart: (String, Int) => Boolean,
      correction: String
    ): Option[String] = {
      var transformed = word
      var lastMatchedBeginIndex = Int.MaxValue

      for (
        endIndex <- word.length to 1 by -1
        if endIndex <= lastMatchedBeginIndex
        if matchPart(word, endIndex)
      ) {
        lastMatchedBeginIndex = endIndex - part.length
        transformed = replace(transformed, lastMatchedBeginIndex, endIndex, correction)
      }

      if (lastMatchedBeginIndex < Int.MaxValue) Some(transformed) else None
    }

    def transform(word: String): Option[String] = {
      transform(word, part, matchWord, correction)
    }

    def inverseTransform(correction: String): Option[String] = {
      transform(correction, this.correction, matchCorrection, part)
    }
  }

  object SyllablePart {

    def apply(part: String, correction: String): SyllablePart = {
      val prefixRequired = part.startsWith("^")
      val suffixRequired = part.endsWith("$")

      SyllablePart(
        part.replaceAll("^\\^|\\$$", ""),
        prefixRequired,
        suffixRequired,
        correction
      )
    }

    def expand(
      shortFormPart: String,
      shortFormCorrection: String
    ): List[SyllablePart] = {
      zipLongest(
        StringExpander(shortFormPart),
        StringExpander(shortFormCorrection)
      ).map {
        case (part, correction) =>
          SyllablePart(part, correction)
      }
    }
  }

  final case class Sequence(
    subrules: List[SyllablePart]
  ) extends CorrectionRule {

    def andThen(other: CorrectionRule): Option[CorrectionRule] = {
      subrules
        .foldRight(Some(other): Option[CorrectionRule]) { (subrule, superRuleOpt) =>
          superRuleOpt.flatMap { superRule =>
            subrule.andThen(superRule)
          }
        }
    }

    def transform(word: String): Option[String] = {
      subrules.foldLeft(Some(word): Option[String]) { (transformedOpt, subrule) =>
        transformedOpt.flatMap { transformed =>
          subrule.transform(transformed)
        }
      }
    }

    def inverseTransform(correction: String): Option[String] = {
      subrules.foldRight(Some(correction) : Option[String]) { (subrule, transformedOpt) =>
        transformedOpt.flatMap { transformed =>
          subrule.inverseTransform(transformed)
        }
      }
    }
  }
}
