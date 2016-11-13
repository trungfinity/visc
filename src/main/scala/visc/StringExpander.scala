package visc

import scala.collection.mutable

object StringExpander extends (String => List[String]) {

  val PartPattern = "(?U)[^\\[\\]]+|\\[[\\w~]*\\]".r
  val OptionPattern = "(?U)(\\w+)~~|(\\w)~|(\\w)".r

  val AccentExpansions = Map(
    'a' -> "aáàảãạ",
    'ă' -> "ăắằẳẵặ",
    'â' -> "âấầẩẫậ",
    'e' -> "eéèẻẽẹ",
    'ê' -> "êếềểễệ",
    'i' -> "iíìỉĩị",
    'o' -> "oóòỏõọ",
    'ô' -> "ôốồổỗộ",
    'ơ' -> "ơớờởỡợ",
    'u' -> "uúùủũụ",
    'ư' -> "ưứừửữự",
    'y' -> "yýỳỷỹỵ"
  )

  private def dfs(
    expansion: String,
    parts: List[String],
    expansionBuilder: mutable.Builder[String, List[String]]
  ): Unit = {
    parts match {
      case firstPart :: remainingParts =>
        if (firstPart.startsWith("[")) {
          OptionPattern
            .findAllMatchIn(firstPart)
            .flatMap { `match` =>
              val needAccents = `match`.matched.endsWith("~")
              val optionsOpt = `match`.subgroups.find(_ != null)
              optionsOpt.map((_, needAccents))
            }
            .toList
            .foreach {
              case (options, needAccents) =>
                options
                  .flatMap { option =>
                    if (needAccents) {
                      AccentExpansions
                        .getOrElse(option, option.toString)
                        .toCharArray

                    } else {
                      List(option)
                    }
                  }
                  .foreach { option =>
                    dfs(expansion + option, remainingParts, expansionBuilder)
                  }
            }

        } else {
          dfs(expansion + firstPart, remainingParts, expansionBuilder)
        }

      case Nil =>
        expansionBuilder += expansion
    }
  }

  def apply(shortForm: String): List[String] = {
    val parts = PartPattern
      .findAllMatchIn(shortForm)
      .map(_.matched)
      .toList

    val expansionBuilder = List.newBuilder[String]

    dfs("", parts, expansionBuilder)

    expansionBuilder.result()
  }
}
