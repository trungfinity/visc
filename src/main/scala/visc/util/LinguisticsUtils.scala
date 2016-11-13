package visc.util

object LinguisticsUtils {

  private def expansions(entries: (Char, String)*): Map[Char, List[Char]] = {
    entries
      .toMap
      .mapValues { options =>
        options
          .toCharArray
          .toList
      }
  }

  private def reductions(expansions: Map[Char, List[Char]]): Map[Char, Char] = {
    expansions.flatMap {
      case (original, options) =>
        options.map(_ -> original)
    }
  }

  val ToneExpansions = expansions(
    'a' -> "áàảãạ",
    'ă' -> "ắằẳẵặ",
    'â' -> "ấầẩẫậ",
    'e' -> "éèẻẽẹ",
    'ê' -> "ếềểễệ",
    'i' -> "íìỉĩị",
    'o' -> "óòỏõọ",
    'ô' -> "ốồổỗộ",
    'ơ' -> "ớờởỡợ",
    'u' -> "úùủũụ",
    'ư' -> "ứừửữự",
    'y' -> "ýỳỷỹỵ"
  )

  val ToneReductions = reductions(ToneExpansions)

  val AccentExpansions = expansions(
    'a' -> "ăâ",
    'd' -> "đ",
    'e' -> "ê",
    'o' -> "ôơ",
    'u' -> "ư"
  )

  val AccentReductions = reductions(AccentExpansions)

  val ToneNormalizations = Map(
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

  def expand(
    original: Char,
    isOriginalIncluded: Boolean = true,
    isAccentExpanded: Boolean = false
  ): List[Char] = {
    val accenteds = if (isAccentExpanded) {
      original :: AccentExpansions.getOrElse(original, List.empty)
    } else {
      List(original)
    }

    accenteds.flatMap { accented =>
      val expansions = ToneExpansions.getOrElse(accented, List.empty)

      if (isOriginalIncluded) {
        accented :: expansions
      } else {
        expansions
      }
    }
  }

  def reduce(
    original: Char,
    isAccentReduced: Boolean = false
  ): Char = {
    val toneReduced = ToneReductions.getOrElse(original, original)

    if (isAccentReduced) {
      AccentReductions.getOrElse(toneReduced, toneReduced)
    } else {
      toneReduced
    }
  }

  def normalize(text: String): String = {
    ToneNormalizations.foldLeft(text.toLowerCase) {
      case (normalized, (from, to)) =>
        normalized.replaceAll(from, to)
    }
  }
}
