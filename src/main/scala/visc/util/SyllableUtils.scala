package visc.util

object SyllableUtils {

  private val expand = StringExpander.expandToSet _
  private val expandAll = StringExpander.expandAllToSet _

  val SingleVowels = expand("[aăâeêioôơuưy~~]")
  val FrontVowels = expandAll("[e~]|[ê~]|[i~]|[i~]a|i[ê~]|[y~]|[y~]a|y[ê~]")
  val NonFrontVowels = expandAll("[a~]|[ă~]|[â~]|[o~]|o[o~]|[ô~]|[ơ~]|[u~]|[u~]a|u[ô~]|[ư~]|[ư~]a|ư[ơ~]")
  val Vowels = FrontVowels ++ NonFrontVowels
  val VowelsWithSupport = expandAll("o[a~]|o[ă~]|o[e~]|u[a~]|u[ă~]|u[â~]|u[e~]|u[ê~]|u[i~]|u[o~]|u[ơ~]|u[y~]|u[y~]a|uy[ê~]")
  val VowelGroups = Vowels ++ VowelsWithSupport

  val LeadingCons = "b|c|ch|d|đ|g|gh|gi|h|k|kh|l|m|n|nh|ng|ngh|p|ph|q|r|s|t|th|tr|v|x"
    .split("\\|")
    .toSet

  val TrailingCons = "c|ch|i|m|n|ng|nh|o|p|t|u|y"
    .split("\\|")
    .toSet

  private def disjunctionPattern(options: Set[String]): String = {
    val optionsByLength = options
      .toList
      .sortBy(-_.length)

    s"${optionsByLength.mkString("|")}"
  }

  private val syllablePartPatterns = List(
    s"(${disjunctionPattern(LeadingCons)})?",
    s"(${disjunctionPattern(VowelGroups)})",
    s"(${disjunctionPattern(TrailingCons)})?"
  )

  val SyllablePartsPattern = ("^" + syllablePartPatterns.mkString + "$").r

  val LeadingConsToVowelGroup = Map(
    // TODO: Other leading vowels? Exclude "ue~"?
    "c" -> NonFrontVowels,
    "g" -> (NonFrontVowels ++ expandAll("o[a~]|o[ă~]|o[e~]|u[a~]|u[â~]|u[ê~]|u[i~]|u[ơ~]|u[y~]|u[y~]a|uy[ê~]")),
    "gh" -> FrontVowels,
    "k" -> FrontVowels,
    "ng" -> (NonFrontVowels ++ expandAll("o[a~]|o[ă~]|o[e~]|u[a~]|u[â~]|u[ê~]|u[i~]|u[ơ~]|u[y~]|u[y~]a|uy[ê~]")),
    "ngh" -> FrontVowels,
    "q" -> expandAll("u[a~]|u[ă~]|u[â~]|u[e~]|u[ê~]|u[i~]|u[o~]|u[ô~]|u[ơ~]|u[y~]|u[y~]a|uy[ê~]")
  )

  val TrailingConsToMainVowel = Map(
    // TODO: Review carefully, maybe we missed some combinations which are not in dictionary
    "c" -> (expand("[áạắặấậéẹóọốộúụứự]") ++ "iế|iệ|yế|yệ|oó|oọ|uố|uộ|ướ|ượ".split("\\|").toSet),
    "ch" -> expand("[áạếệíịýỵ]"),
    "i" -> expandAll("[a~]|[o~]|[ô~]|[ơ~]|[u~]|u[ô~]|[ư~]|ư[ơ~]"),
    "m" -> (Vowels -- expandAll("[i~]a|[y~]a|[u~]a|[ư~]a")),
    "n" -> (Vowels -- expandAll("[i~]a|[y~]a|[u~]a|[ư~]a")),
    "ng" -> (expandAll("[e~]|i[ê~]|y[ê~]") ++ NonFrontVowels -- expandAll("[ơ~]|[u~]a|[ư~]a")),
    "nh" -> expandAll("[ê~]|[i~]|[y~]|[a~]"),
    "o" -> expandAll("[e~]|[a~]"),
    "p" -> (expand("[áạắặấậéẹếệíịóọốộớợúụứựýỵ]") ++ expandAll("iế|iệ|yế|yệ|uố|uộ|ướ|ượ")),
    "t" -> (expand("[áạắặấậéẹếệíịóọốộớợúụứựýỵ]") ++ expandAll("iế|iệ|yế|yệ|uố|uộ|ướ|ượ")),
    "u" -> expandAll("[ê~]|[i~]|i[ê~]|[y~]|y[ê~]|[a~]|[â~]|[ư~]|ư[ơ~]"),
    "y" -> expand("[aâu~~]")
  )

  val TrailRequiredVowels = expandAll("i[ê~]|y[ê~]|[ă~]|[â~]|o[o~]|u[ô~]|ư[ơ~]")

  // TODO: Handle vowel group cases, like "u[ơ~]"
  val NoTrailVowels = expandAll("[i~]a|[y~]a|[u~]a|[ư~]a")

  private val iExpanded = expand("[i~]")
  private val e2Expanded = expand("[ê~]")

  private def leadingConsAndVowelGroup(
    leadingConsMatch: Option[String],
    vowelGroupMatch: String,
    trailingCons: Option[String]
  ): Option[(Option[String], String)] = {
    if (leadingConsMatch.contains("g") && iExpanded.contains(vowelGroupMatch)) {
      if (!trailingCons.exists(SingleVowels.contains)) {
        Some((Some("gi"), vowelGroupMatch))
      } else {
        None
      }
    } else if (leadingConsMatch.contains("gi") && e2Expanded.contains(vowelGroupMatch) && trailingCons.isDefined) {
      Some((Some("gi"), "i" + vowelGroupMatch))

    } else {
      Some((leadingConsMatch, vowelGroupMatch))
    }
  }

  final case class SyllableParts(
    leadingCons: Option[String],
    supportingVowel: Option[String],
    mainVowel: String,
    trailingCons: Option[String]
  )

  def split(
    syllable: String,
    isFullSyllable: Boolean = true
  ): Option[SyllableParts] = {
    val normalized = LinguisticsUtils.normalize(syllable)

    for {
      match0 <- SyllablePartsPattern.findFirstMatchIn(normalized)

      trailingCons = Option(match0.group(3))
      (leadingCons, vowelGroup) <- leadingConsAndVowelGroup(
        Option(match0.group(1)),
        match0.group(2),
        trailingCons
      )

      isVowel = Vowels.contains(vowelGroup)
      isVowelWithSupport = VowelsWithSupport.contains(vowelGroup)

      hasSupportingVowel = if (isVowel && isVowelWithSupport) { // "ua"
        leadingCons.contains("q")
      } else {
        isVowelWithSupport
      }

      (supportingVowel, mainVowel) = if (hasSupportingVowel) {
        (vowelGroup.headOption.map(_.toString), vowelGroup.drop(1))
      } else {
        (None, vowelGroup)
      }

      isLeadingConsValid = leadingCons.fold(true) { leadingCons =>
        LeadingConsToVowelGroup
          .get(leadingCons)
          .fold(true) { requirements =>
            requirements.contains(vowelGroup)
          }
      }

      isMainVowelValid = !isFullSyllable || {
        if (TrailRequiredVowels.contains(mainVowel)) {
          trailingCons.nonEmpty
        } else if (NoTrailVowels.contains(mainVowel)) {
          trailingCons.isEmpty
        } else {
          true
        }
      }

      isTrailingConsValid = trailingCons.fold(true) { trailingCons =>
        TrailingConsToMainVowel
          .get(trailingCons)
          .fold(true) { requirements =>
            requirements.contains(mainVowel)
          }
      }

      valid = isLeadingConsValid && isMainVowelValid && isTrailingConsValid

      syllableParts <- if (valid) {
        Some(SyllableParts(leadingCons, supportingVowel, mainVowel, trailingCons))
      } else {
        None
      }
    } yield syllableParts
  }

  def validate(syllable: String, isFullSyllable: Boolean = true): Boolean = {
    if (!isFullSyllable && (LeadingCons.contains(syllable) || TrailingCons.contains(syllable))) {
      true
    } else {
      split(syllable, isFullSyllable).isDefined
    }
  }

  def validateText(text: String): Boolean = {
    text
      .split("\\s+")
      .forall(validate(_))
  }
}
