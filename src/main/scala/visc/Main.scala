package visc

import scala.collection.mutable

object Main extends App {

  import CorrectionRule._
  import CorrectionType._

  type Expander = (String, String, CorrectionType) => List[(Basic, CorrectionType)]

  def expander(
    build: (String, String) => List[Basic]
  ): Expander = { (textShortForm, correctionShortForm, correctionType) =>
    build(textShortForm, correctionShortForm).map { rule =>
      rule -> correctionType
    }
  }

  val expandWord = expander(Word.expand)
  val expandSyllablePart = expander(SyllablePart.expand)

  def expandWords(
    params: ((String, String), CorrectionType)*
  ): List[(Basic, CorrectionType)] = {
    params.toList.flatMap {
      case ((word, correction), correctionType) =>
        expandWord(word, correction, correctionType)
    }
  }

  def expandSyllableParts(
    params: ((String, String), CorrectionType)*
  ): List[(Basic, CorrectionType)] = {
    params.toList.flatMap {
      case ((part, correction), correctionType) =>
        expandSyllablePart(part, correction, correctionType)
    }
  }

  val elementaryRuleBuilder = Set.newBuilder[(Basic, CorrectionType)]

  elementaryRuleBuilder ++= expandSyllableParts(
    ("a", "[aăâ~~]") -> Accent,
    ("ă", "[ă~]") -> Accent,
    ("â", "[â~]") -> Accent,
    ("e", "[eê~~]") -> Accent,
    ("ê", "[ê~]") -> Accent,
    ("i", "[i~]") -> Accent,
    ("o", "[oôơ~~]") -> Accent,
    ("ô", "[ô~]") -> Accent,
    ("ơ", "[ơ~]") -> Accent,
    ("u", "[uư~~]") -> Accent,
    ("ư", "[ư~]") -> Accent,
    ("y", "[y~]") -> Accent
  )

  elementaryRuleBuilder ++= List(
    SyllablePart("d", "đ") -> Accent
  )

  elementaryRuleBuilder ++= List(
    Word("ih", "đi") -> FullWord,
    Word("hoy", "thôi") -> FullWord,

    SyllablePart("g$", "ng") -> ShortForm,
    SyllablePart("h$", "nh") -> ShortForm,

    Word("a", "anh") -> ShortForm,
    Word("e", "em") -> ShortForm,
    Word("b", "bạn") -> ShortForm,
    Word("m", "mình") -> ShortForm,
    Word("m", "mày") -> ShortForm,
    Word("t", "tao") -> ShortForm,

    Word("k", "không") -> ShortForm,
    Word("ko", "không") -> ShortForm
  )

  elementaryRuleBuilder ++= expandSyllableParts(
    ("^ng[i~]", "ngh[i~]") -> TeenCode,
    ("^ng[e~]", "ngh[e~]") -> TeenCode,
    ("^g[e~]", "gh[e~]") -> TeenCode
  )

  elementaryRuleBuilder ++= List(
    SyllablePart("^ck", "ch") -> TeenCode,
    SyllablePart("^gk", "gh") -> TeenCode,
    SyllablePart("^nk", "nh") -> TeenCode,
    SyllablePart("^pk", "ph") -> TeenCode,
    SyllablePart("^tk", "th") -> TeenCode,

    SyllablePart("ck$", "ch") -> TeenCode,
    SyllablePart("nk$", "nh") -> TeenCode,
    SyllablePart("k$", "ch") -> TeenCode,
    SyllablePart("k$", "nh") -> TeenCode,
    SyllablePart("k$", "c") -> TeenCode
  )

  elementaryRuleBuilder ++= expandSyllableParts(
    ("j", "[i~]") -> TeenCode,
    ("[i~][cmnptu]$", "i[ê~][cmnptu]") -> TeenCode,
    ("[i~]ng$", "i[ê~]ng") -> TeenCode
  )

  elementaryRuleBuilder ++= List(
    SyllablePart("^k", "c") -> TeenCode,
    SyllablePart("^j", "gi") -> TeenCode,
    SyllablePart("^j$", "gì") -> TeenCode
  )

  def isRuleValid(rule: CorrectionRule): Boolean = {
    rule match {
      case Word(_, correction) =>
        SyllableValidator(correction)

      case SyllablePart(_, _, _, correction) =>
        SyllableValidator(correction, isFullSyllable = false)

      case Sequence(subrules) =>
        subrules.forall {
          case SyllablePart(_, _, _, correction) =>
            SyllableValidator(correction, isFullSyllable = false)
        }
    }
  }

  def correctionTypeCost(correctionType: CorrectionType): Double = {
    correctionType match {
      case Insertion => 1
      case Deletion => 1
      case Replacement => 1
      case Transposition => 1

      case FullWord => .75
      case Accent => .35
      case ShortForm => .25
      case TeenCode => .35
    }
  }

  val elementaryRules = elementaryRuleBuilder
    .result()
    .filter {
      case (rule, _) =>
        isRuleValid(rule)
    }
    .map {
      case (rule, correctionType) =>
        rule -> correctionTypeCost(correctionType)
    }

  println(elementaryRules.size)

  val compoundRules = mutable.Map.empty[CorrectionRule, Double]

  def dfs(
    depth: Int,
    rule: CorrectionRule,
    cost: Double
  ): Unit = {
    if (depth > 0) {
      compoundRules += rule -> cost
    }

    if (depth < 3) {
      for (
        (elementaryRule, elementaryCost) <- elementaryRules
      ) {
        rule.andThen(elementaryRule).foreach { newRule =>
          val existingCost = compoundRules.getOrElse(newRule, Double.MaxValue)
          val newCost = cost + elementaryCost

          if (newCost < existingCost && isRuleValid(newRule)) {
            dfs(depth + 1, newRule, newCost)
          }
        }
      }
    }
  }

  dfs(0, Sequence(Nil), 0)

  println(compoundRules.size)

  val basicRules = (compoundRules.toSet ++ elementaryRules)
    .filter {
      case (_: Basic, _) => true
      case (_, _) => false
    }
    .map {
      case (rule, correctionTypes) =>
        rule.asInstanceOf[Basic] -> correctionTypes
    }

  println(basicRules.size)

  def correctionCost(
    word: String,
    correction: String
  ): Double = {
    val minCost = Array.ofDim[Double](word.length + 1, correction.length + 1)

    for (i <- 0 to word.length) minCost(i)(0) = i * correctionTypeCost(Deletion)
    for (i <- 1 to correction.length) minCost(0)(i) = i * correctionTypeCost(Insertion)

    for {
      i <- 1 to word.length
      j <- 1 to correction.length
    } {
      val wordChar = word.charAt(i - 1)
      val correctionChar = correction.charAt(j - 1)

      minCost(i)(j) = Math.min(
        minCost(i - 1)(j) + correctionTypeCost(Deletion),
        minCost(i)(j - 1) + correctionTypeCost(Insertion)
      )

      for (
        (rule, cost) <- basicRules
        if rule.matchTransform(word, i, correction, j)
      ) {
        val (wordBeginIndex, correctionBeginIndex) = rule.beginIndices(i, j)

        minCost(i)(j) = Math.min(
          minCost(i)(j),
          minCost(wordBeginIndex)(correctionBeginIndex) + cost
        )
      }

      val replacementCost = if (wordChar != correctionChar) {
        correctionTypeCost(Replacement)
      } else {
        0
      }

      minCost(i)(j) = Math.min(minCost(i)(j), minCost(i - 1)(j - 1) + replacementCost)

      if (i >= 2 && j >= 2) {
        val prevWordChar = word.charAt(i - 2)
        val prevCorrectionChar = correction.charAt(j - 2)

        if (wordChar == prevCorrectionChar && prevWordChar == correctionChar) {
          minCost(i)(j) = Math.min(minCost(i)(j), minCost(i - 2)(j - 2) + correctionTypeCost(Transposition))
        }
      }
    }

    println(s"$word -> $correction: ${minCost(word.length)(correction.length)}")

    minCost(word.length)(correction.length)
  }

  correctionCost("san pham", "sản phẩm")
  correctionCost("k", "không")
  correctionCost("ko", "không")
  correctionCost("a", "anh")
  correctionCost("e", "em")
  correctionCost("t", "tao")
  correctionCost("m", "mày")
  correctionCost("oke", "ok")
  correctionCost("nhiu", "nhiều")
  correctionCost("nhìu", "nhiều")
  correctionCost("nhiu", "nhiêu")
  correctionCost("nhju", "nhiêu")
  correctionCost("nhju", "nhiều")
  correctionCost("gja", "giá")

  compoundRules.foreach { rule =>
    rule._1.transform("mjk").foreach { correction =>
      if (SyllableValidator(correction)) {
        println(rule)
        println(correction)
      }
    }
  }

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
