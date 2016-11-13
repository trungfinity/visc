package visc.correction

import visc.util.{ListUtils, StringExpander}

sealed abstract class CorrectionRule extends Product with Serializable {

  def andThen(other: CorrectionRule): Option[CorrectionRule]

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

  def transform(word: String): Option[String]
  def inverseTransform(correction: String): Option[String]
}

object CorrectionRule extends CorrectionRules {

  private def replace(
    word: String,
    beginIndex: Int,
    endIndex: Int,
    replacement: String
  ): String = {
    word.substring(0, beginIndex) + replacement + word.substring(endIndex)
  }

  final case class Word(
    word: String,
    correction: String
  ) extends CorrectionRule {

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
      wordText: String,
      correctionText: String
    ): List[Word] = {
      ListUtils.zipLongest(
        StringExpander.expand(wordText),
        StringExpander.expand(correctionText)
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
  ) extends CorrectionRule {

    def andThen(other: CorrectionRule): Option[CorrectionRule] = {
      other match {
        case rule: Word =>
          inverseTransform(rule.word)
            .map(Word(_, rule.correction))

        case rule: SyllablePart =>
          // We relax the condition in order to merge rules aggressively
          /* if ((prefixRequired && rule.prefixRequired) || (suffixRequired && rule.suffixRequired)) { */
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

          /* } else {
            val bothInMiddle = !(prefixRequired || suffixRequired ||
              rule.prefixRequired || rule.suffixRequired)

            if (bothInMiddle && correction == rule.part) {
              Some(SyllablePart(part, rule.correction))
            } else {
              None
            }
          } */
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
      partText: String,
      correctionText: String
    ): List[SyllablePart] = {
      ListUtils.zipLongest(
        StringExpander.expand(partText),
        StringExpander.expand(correctionText)
      ).map {
        case (part, correction) =>
          SyllablePart(part, correction)
      }
    }
  }
}
