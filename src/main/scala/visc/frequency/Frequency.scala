package visc.frequency

import java.io.FileNotFoundException

import scala.collection.mutable
import scala.io.Source

object Frequency {

  private val dictionary = mutable.ListBuffer.empty[String]
  private val dictionaryIndices = mutable.Map.empty[String, Int]
  private val inverseDictionaryIndices = mutable.Map.empty[Int, String]

  private val unigramTermFrequency = mutable.Map.empty[Int, Int]
  private val bigramTermFrequency = mutable.Map.empty[(Int, Int), Int]

  private val unigramDocFrequency = mutable.Map.empty[Int, Int]
  private val bigramDocFrequency = mutable.Map.empty[(Int, Int), Int]

  try {
    Source
      .fromFile(s"${FrequencyDirs.FrequencyDir}/dictionary.txt")
      .getLines()
      .foreach { word =>
        dictionaryIndices += word -> dictionary.size
        inverseDictionaryIndices += dictionary.size -> word
        dictionary += word
      }

  } catch {
    case _: FileNotFoundException =>
    case e: Throwable => throw e
  }

  val Dictionary = dictionary.toList
  val DictionaryIndices = dictionaryIndices.toMap
  val InverseDictionaryIndices = inverseDictionaryIndices.toMap

  private def readUnigrams(path: String, unigramFrequency: mutable.Map[Int, Int]): Unit = {
    try {
      Source
        .fromFile(path)
        .getLines()
        .foreach { line =>
          val parts = line.split(" ").map(_.toInt)
          unigramFrequency += parts(0) -> parts(1)
        }

    } catch {
      case _: FileNotFoundException =>
      case e: Throwable => throw e
    }
  }

  private def readBigrams(path: String, bigramFrequency: mutable.Map[(Int, Int), Int]): Unit = {
    try {
      Source
        .fromFile(path)
        .getLines()
        .foreach { line =>
          val parts = line.split(" ").map(_.toInt)
          bigramFrequency += (parts(0), parts(1)) -> parts(2)
        }

    } catch {
      case _: FileNotFoundException =>
      case e: Throwable => throw e
    }
  }

  readUnigrams(s"${FrequencyDirs.FrequencyDir}/term-unigram.txt", unigramTermFrequency)
  readBigrams(s"${FrequencyDirs.FrequencyDir}/term-bigram.txt", bigramTermFrequency)
  // readUnigrams(s"${FrequencyDirs.FrequencyDir}/doc-unigram.txt", unigramDocFrequency)
  // readBigrams(s"${FrequencyDirs.FrequencyDir}/doc-bigram.txt", bigramDocFrequency)

  val UnigramTermFrequency = unigramTermFrequency.toMap
  val BigramTermFrequency = bigramTermFrequency.toMap
  val UnigramDocFrequency = unigramDocFrequency.toMap
  val BigramDocFrequency = bigramDocFrequency.toMap
}
