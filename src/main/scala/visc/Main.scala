package visc

import scala.io.StdIn

import visc.correction.SpellingCorrection

object Main extends App {

  val spellingCorrection = new SpellingCorrection

  def debug(word: String, correction: String): Unit = {
    printf("%s -> %s:\n", word, correction)
    printf("Cost: %.2f\n", spellingCorrection.correctionCost(word, correction))
    printf("Probability: %.2f\n", spellingCorrection.correctionProbability(word, correction))
    printf("Frequency: %.2f\n", spellingCorrection.frequencyRatio(correction))
    printf("Score: %.2f\n", spellingCorrection.correctionScore(word, correction))
  }

//  debug("e", "em")
//  debug("e", "e")
//
//  debug("hoj", "hơi")
//  debug("hoj", "hỏi")

  while (true) {
    print("Input a sentence: ")

    val sentence = StdIn.readLine()

    val time = System.currentTimeMillis()
    val correction = spellingCorrection.sentenceCorrection(sentence)

    printf("Took %.2f seconds.\n", (System.currentTimeMillis() - time).toDouble / 1000)
    printf("Correction: %s\n", correction)
  }
}
