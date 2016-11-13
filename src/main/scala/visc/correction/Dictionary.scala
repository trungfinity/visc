package visc.correction

import scala.io.Source

import visc.util.{LinguisticsUtils, SyllableUtils}

object Dictionary {

  val WordPattern = "(?U)^[\\w ]+$".r
  val InvalidSingleWords = "ăâbcdđfghjklmnpqrstvxz"
    .toCharArray
    .map(_.toString)
    .toSet

  val Words = Source
    .fromFile("data/Viet74K.txt")
    .getLines()
    .flatMap { word =>
      val normalized = LinguisticsUtils.normalize(word)

      val hasStrangeChars = WordPattern.findFirstIn(normalized).isEmpty
      val invalidSingleChar = InvalidSingleWords.contains(normalized)
      val numSyllable = normalized.count(_ == ' ') + 1

      if (hasStrangeChars || invalidSingleChar || numSyllable >= 3 ||
        !SyllableUtils.validateText(normalized)) {
        None

      } else {
        Some(normalized)
      }
    }
    .toList
    .distinct

  val WordSet = Words.toSet
}
