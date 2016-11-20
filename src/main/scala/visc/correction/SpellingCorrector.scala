package visc.correction

abstract class SpellingCorrector {
  def corrections(word: String): Set[String]
  def distance(word: String, correction: String): Double
}
