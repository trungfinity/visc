package visc.util

import scala.collection.mutable

object StringExpander {

  private val PartPattern = "(?U)[^\\[\\]]+|\\[[\\w~]*\\]".r
  private val OptionPattern = "(?U)(\\w+)~~|(\\w)~|(\\w)".r

  private def expand(
    parts: List[String],
    expansion: String,
    expansionBuilder: mutable.Builder[String, List[String]]
  ): Unit = {
    parts match {
      case firstPart :: remainingParts =>
        if (firstPart.startsWith("[")) {
          for {
            match0 <- OptionPattern.findAllMatchIn(firstPart)

            options <- match0.subgroups.find(_ != null)
            needAccents = match0.matched.endsWith("~")

            option <- if (needAccents) {
              options.flatMap(LinguisticsUtils.expand(_))
            } else {
              options
            }
          } expand(remainingParts, expansion + option, expansionBuilder)

        } else {
          expand(remainingParts, expansion + firstPart, expansionBuilder)
        }

      case Nil =>
        expansionBuilder += expansion
    }
  }

  private def expand(parts: List[String]): List[String] = {
    val expansionBuilder = List.newBuilder[String]
    expand(parts, "", expansionBuilder)
    expansionBuilder.result()
  }

  def expand(text: String): List[String] = {
    val parts = PartPattern
      .findAllMatchIn(text)
      .map(_.matched)
      .toList

    expand(parts)
  }

  def expandAll(text: String): List[String] = {
    text
      .split("\\|")
      .flatMap(expand)
      .toList
  }

  def expandToSet(text: String): Set[String] = {
    expand(text).toSet
  }

  def expandAllToSet(text: String): Set[String] = {
    expandAll(text).toSet
  }
}
