package visc.util

object ListUtils {

  def zipLongest[T](firstList : List[T], secondList : List[T]): List[(T, T)] = {
    if (firstList.size <= secondList.size) {
      Stream
        .continually(firstList)
        .flatten
        .zip(secondList)
        .toList

    } else {
      zipLongest(secondList, firstList)
        .map(_.swap)
    }
  }
}
