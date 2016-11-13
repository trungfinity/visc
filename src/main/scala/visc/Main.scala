package visc

import org.apache.commons.math3.distribution.NormalDistribution
import visc.correction.{Correction, CorrectionRule}

object Main extends App {

  val normalDistribution = new NormalDistribution(1, .4)

  def correct(word: String, expected: String): Unit = {
    Correction
      .correct(word)
      .foreach {
        case (correction, cost) =>
          val probability = 1 - normalDistribution.cumulativeProbability(cost)

          if ((word.length > 2 && probability >= .15) || probability > .5) {
            printf("%s -> %s: %.2f\n", word, correction, probability)
          }
      }
  }

  printf("Elementary: %d rules.\n", CorrectionRule.Elements.size)
  printf("Basic: %d rules.\n", CorrectionRule.Basics.size)

  correct("san pham", "sản phẩm")
  correct("k", "không")
  correct("ko", "không")
  correct("a", "anh")
  correct("e", "em")
  correct("t", "tao")
  correct("m", "mày")
  correct("oke", "ok")
  correct("nhiu", "nhiều")
  correct("nhìu", "nhiều")
  correct("nhiu", "nhiêu")
  correct("nhju", "nhiêu")
  correct("nhju", "nhiều")
  correct("gja", "giá")
  correct("zá", "giá")
  correct("zì", "gì")
  correct("zậy", "vậy")

//  Correction
//    .expand("san pham")
//    .find(_ == "sản phẩm")
//    .foreach(println)
//
//  println(Dictionary.Entries)
//
//  println(Correction.suggest("mjk"))
//  println(Correction.suggest("san pham"))
//
//  val rule = CorrectionRule.SyllablePart("g", prefixRequired = false, suffixRequired = true, "ng")
//  val rule2 = CorrectionRule.SyllablePart("g", prefixRequired = false, suffixRequired = false, "gg")
//  val rule3 = CorrectionRule.Word("mừng", "crazy")
//
//  println(rule.transform("mừg"))
//  println(rule.inverseTransform("mừng"))
//  println(rule2.transform("ggg"))
//  println(rule2.inverseTransform("gggggg"))
//  println(rule.andThen(rule3).flatMap(_.transform("mừg")))
//
//  val rule4 = CorrectionRule.SyllablePart("j", "i")
//  val rule5 = CorrectionRule.SyllablePart("k$", "h")
//  val rule6 = CorrectionRule.SyllablePart("h$", "nh")
//  val rule7 = CorrectionRule.SyllablePart("i", "ì")
//  val rule8 = CorrectionRule.Word("mình", "tớ")
//
//  for {
//    rule45 <- rule4.andThen(rule5)
//    _ = println("45 " + rule45)
//    rule456 <- rule45.andThen(rule6)
//    _ = println("456 " + rule456)
//    rule4567 <- rule456.andThen(rule7)
//    _ = println("4567 " + rule4567)
//    rule45678 <- rule4567.andThen(rule8)
//    _ = println("45678 " + rule45678)
//  } {
//    println(rule4567.inverseTransform("mình"))
//    println(rule45678.transform("mjk"))
//  }
}
