package visc.util

import scala.io.Source

object Dictionary {

  val WordPattern = "(?U)^[\\w ]+$".r
  val InvalidSingleCharWords = "ăâbcdđfghjklmnpqrstvxz"
    .toCharArray
    .map(_.toString)
    .toSet

  val AdditionalWords = List(
    "admin",
    "cường lực"
  )

  private val lines = Source
    .fromFile("data/dictionary/Viet74K.txt")
    .getLines()

  val Words = (lines ++ AdditionalWords)
    .flatMap { word =>
      val normalized = LinguisticsUtils.normalize(word)

      val hasStrangeChars = WordPattern.findFirstIn(normalized).isEmpty
      val invalidSingleChar = InvalidSingleCharWords.contains(normalized)
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
}
