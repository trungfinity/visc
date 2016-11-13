package visc

object SyllableValidator extends (String => Boolean) {

  private def expand(shortForm: String): Set[String] = {
    StringExpander(shortForm).toSet
  }

  private def expandAll(shortForm: String): Set[String] = {
    shortForm
      .split("\\|")
      .flatMap(StringExpander)
      .toSet
  }

  val SingleVowels = expand("[aăâeêioôơuưy~~]")
  val FrontVowels = expandAll("[e~]|[ê~]|[i~]|[i~]a|i[ê~]|[y~]|[y~]a")
  val NonFrontVowels = expandAll("[a~]|[ă~]|[â~]|[o~]|o[o~]|[ô~]|[ơ~]|[u~]|[u~]a|u[ô~]|[ư~]|[ư~]a|ư[ơ~]")
  val Vowels = FrontVowels ++ NonFrontVowels
  val VowelsWithSupport = expandAll("o[a~]|o[ă~]|o[e~]|u[a~]|u[â~]|u[e~]|u[ê~]|u[i~]|u[ơ~]|u[y~]|u[y~]a")
  val VowelGroups = Vowels ++ VowelsWithSupport

  val I = expand("[i~]")
  val E2 = expand("[ê~]")

  val LeadingCons = "b|c|ch|d|đ|g|gh|gi|h|k|kh|l|m|n|nh|ng|ngh|ph|q|r|s|t|th|tr|v|x"
    .split("\\|")
    .toSet

  val TrailingCons = "c|ch|i|m|n|ng|nh|o|p|t|u|y"
    .split("\\|")
    .toSet

  def disjunctionPattern(options: Set[String]): String = {
    val optionsByLength = options
      .toList
      .sortBy(-_.length)

    s"${optionsByLength.mkString("|")}"
  }

  val SyllablePartPatterns = List(
    s"(${disjunctionPattern(LeadingCons)})?",
    s"(${disjunctionPattern(VowelGroups)})",
    s"(${disjunctionPattern(TrailingCons)})?"
  )

  val SyllablePartsPattern = ("^" + SyllablePartPatterns.mkString + "$").r

  val LeadAndGroupRequirements = Map(
    // TODO: Other leading vowels? Exclude "ue~"?
    "c" -> NonFrontVowels,
    "g" -> (NonFrontVowels ++ expandAll("o[a~]|o[ă~]|o[e~]|u[a~]|u[â~]|u[e~]|u[ê~]|u[i~]|u[ơ~]|u[y~]|u[y~]a")),
    "gh" -> FrontVowels,
    "k" -> FrontVowels,
    "ng" -> (NonFrontVowels ++ expandAll("o[a~]|o[ă~]|o[e~]|u[a~]|u[â~]|u[e~]|u[ê~]|u[i~]|u[ơ~]|u[y~]|u[y~]a")),
    "ngh" -> FrontVowels,
    "q" -> expandAll("u[a~]|u[â~]|u[e~]|u[i~]|u[ê~]|u[ô~]|u[ơ~]|u[y~]|u[y~]a")
  )

  val TrailAndMainRequirements = Map(
    // TODO: Review carefully, maybe we missed some combinations which are not in dictionary
    "c" -> (expand("[áạắặấậéẹóọốộúụứự]") ++ "iế|iệ|oó|oọ|uố|uộ|ướ|ượ".split("\\|").toSet),
    "ch" -> expand("[áạếệíịýỵ]"),
    "i" -> expandAll("[a~]|[o~]|[ô~]|[ơ~]|[u~]|u[ô~]|[ư~]|ư[ơ~]"),
    "m" -> (Vowels -- expandAll("[i~]a|[y~]a|[u~]a|[ư~]a")),
    "n" -> (Vowels -- expandAll("[i~]a|[y~]a|[u~]a|[ư~]a")),
    "ng" -> (expandAll("[e~]|i[ê~]") ++ NonFrontVowels -- expandAll("[ơ~]|[u~]a|[ư~]a")),
    "nh" -> expandAll("[ê~]|[i~]|[y~]|[a~]"),
    "o" -> expandAll("[e~]|[a~]"),
    "p" -> (expand("[áạắặấậéẹếệíịóọốộớợúụứựýỵ]") ++ expandAll("iế|iệ|ướ|ượ")),
    "t" -> (expand("[áạắặấậéẹếệíịóọốộớợúụứựýỵ]") ++ expandAll("iế|iệ|uố|uộ|ướ|ượ")),
    "u" -> expandAll("[ê~]|[i~]|i[ê~]|[y~]|[a~]|[â~]|[ư~]|ư[ơ~]"),
    "y" -> expand("[aâu~~]")
  )

  val TrailRequiredVowels = expandAll("i[ê~]|[ă~]|[â~]|o[o~]|u[ô~]|ư[ơ~]")
  val NoTrailVowels = expandAll("[i~]a|[y~]a|[u~]a|[ư~]a")

  final case class SyllableParts(
    leadingCons: Option[String],
    supportingVowel: Option[String],
    mainVowel: String,
    trailingCons: Option[String]
  )

  private def leadingConsAndVowelGroup(
    leadingConsMatch: Option[String],
    vowelGroupMatch: String,
    trailingCons: Option[String]
  ): Option[(Option[String], String)] = {
    if (leadingConsMatch.contains("g") && I.contains(vowelGroupMatch)) {
      if (!trailingCons.exists(SingleVowels.contains)) {
        Some((Some("gi"), vowelGroupMatch))
      } else {
        None
      }
    } else if (leadingConsMatch.contains("gi") && E2.contains(vowelGroupMatch) && trailingCons.isDefined) {
      Some((Some("gi"), "i" + vowelGroupMatch))

    } else {
      Some((leadingConsMatch, vowelGroupMatch))
    }
  }

  val NormalizationRules = Map(
    "òa" -> "oà",
    "óa" -> "oá",
    "ỏa" -> "oả",
    "õa" -> "oã",
    "ọa" -> "oạ",
    "òe" -> "oè",
    "óe" -> "oé",
    "ỏe" -> "oẻ",
    "õe" -> "oẽ",
    "ọe" -> "oẹ",
    "ùy" -> "uỳ",
    "úy" -> "uý",
    "ủy" -> "uỷ",
    "ũy" -> "uỹ",
    "ụy" -> "uỵ"
  )

  private def normalize(syllable: String): String = {
    NormalizationRules.foldLeft(syllable.toLowerCase) {
      case (normalized, (from, to)) =>
        normalized.replaceAll(from, to)
    }
  }

  def split(syllable: String, isFullSyllable: Boolean = true): Option[SyllableParts] = {
    val normalized = normalize(syllable)

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
        LeadAndGroupRequirements
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
        TrailAndMainRequirements
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

  def apply(text: String, isFullSyllable: Boolean): Boolean = {
    text
      .split("\\s+")
      .forall { text =>
        if (!isFullSyllable && (LeadingCons.contains(text) || TrailingCons.contains(text))) {
          true
        } else {
          split(text, isFullSyllable).isDefined
        }
      }
  }

  def apply(text: String): Boolean = {
    apply(text, isFullSyllable = true)
  }
}
