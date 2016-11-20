package visc.frequency

import java.io.{BufferedWriter, File, FileWriter, PrintWriter}

import scala.collection.mutable
import scala.io.Source

import visc.util.{LinguisticsUtils, SyllableUtils}

object FrequencyProcessor extends App {

  private val dictionary = mutable.ListBuffer.empty[String]
  private val dictionaryIndices = mutable.Map.empty[String, Int]
  private val inverseDictionaryIndices = mutable.Map.empty[Int, String]

  private val unigramTermFrequency = mutable.Map.empty[Int, Int]
  private val bigramTermFrequency = mutable.Map.empty[(Int, Int), Int]

  private val unigramDocFrequency = mutable.Map.empty[Int, Int]
  private val bigramDocFrequency = mutable.Map.empty[(Int, Int), Int]

  private val articleIndices = new File(FrequencyDirs.TaggedDir)
    .list()
    .map(_.toInt)
    .sorted

  articleIndices
    .foreach { articleIndex =>
      if ((articleIndex + 1) % 100 == 0) {
        printf("Preprocessing %d articles..\n", articleIndex + 1)
      }

      val articlePath = s"${FrequencyDirs.TaggedDir}/$articleIndex"
      val lines = Source.fromFile(articlePath).getLines().toArray
      val words = lines.map { line =>
        val parts = line.split("/")
        val word = parts.dropRight(1).mkString("/")
        LinguisticsUtils.normalize(word)
      }

      val wordValidity = words.map { word =>
        SyllableUtils.validateText(word)
      }

      val unigrams = mutable.Set.empty[Int]
      val bigrams = mutable.Set.empty[(Int, Int)]

      for (i <- words.indices) {
        val word = words(i)

        if (wordValidity(i)) {
          if (!dictionaryIndices.contains(word)) {
            dictionaryIndices += word -> dictionary.size
            inverseDictionaryIndices += dictionary.size -> word
            dictionary += word
          }

          val dictionaryIndex = dictionaryIndices(word)
          val unigramFrequency = unigramTermFrequency.getOrElse(dictionaryIndex, 0)
          unigramTermFrequency += dictionaryIndex -> (unigramFrequency + 1)

          if (!unigrams.contains(dictionaryIndex)) {
            val unigramFrequency = unigramDocFrequency.getOrElse(dictionaryIndex, 0)
            unigramDocFrequency += dictionaryIndex -> (unigramFrequency + 1)
            unigrams += dictionaryIndex
          }

          if (i > 0 && wordValidity(i - 1)) {
            val prevWord = words(i - 1)
            val prevWordDictionaryIndex = dictionaryIndices(prevWord)
            val index = (prevWordDictionaryIndex, dictionaryIndex)
            val bigramFrequency = bigramTermFrequency.getOrElse(index, 0)
            bigramTermFrequency += index -> (bigramFrequency + 1)

            if (!bigrams.contains(index)) {
              val bigramFrequency = bigramDocFrequency.getOrElse(index, 0)
              bigramDocFrequency += index -> (bigramFrequency + 1)
              bigrams += index
            }
          }
        }
      }
    }

  printf("Processed %d articles.\n", articleIndices.length)

  printf("Uni-gram: %d grams.\n", unigramTermFrequency.size)
  printf("Bi-gram: %d grams.\n", bigramTermFrequency.size)

  println("Writing result files..")

  private def writeFile(path: String, lines: List[String]): Unit = {
    val writer = new PrintWriter(new BufferedWriter(new FileWriter(path)))
    lines.foreach(writer.println)
    writer.close()
  }

  private def flattenUnigrams(unigramFrequency: mutable.Map[Int, Int]): List[String] = {
    unigramFrequency
      .toList
      .sortBy(-_._2)
      .map {
        case (index, frequency) =>
          s"$index $frequency"
      }
  }

  private def flattenBigrams(bigramFrequency: mutable.Map[(Int, Int), Int]): List[String] = {
    bigramFrequency
      .toList
      .sortBy(-_._2)
      .map {
        case ((firstIndex, secondIndex), frequency) =>
          s"$firstIndex $secondIndex $frequency"
      }
  }

  writeFile(s"${FrequencyDirs.FrequencyDir}/dictionary.txt", dictionary.toList)
  writeFile(s"${FrequencyDirs.FrequencyDir}/term-unigram.txt", flattenUnigrams(unigramTermFrequency))
  writeFile(s"${FrequencyDirs.FrequencyDir}/term-bigram.txt", flattenBigrams(bigramTermFrequency))
  writeFile(s"${FrequencyDirs.FrequencyDir}/doc-unigram.txt", flattenUnigrams(unigramDocFrequency))
  writeFile(s"${FrequencyDirs.FrequencyDir}/doc-bigram.txt", flattenBigrams(bigramDocFrequency))

  println("Done.")
}
