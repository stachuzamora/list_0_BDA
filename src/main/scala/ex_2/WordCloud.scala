package ex_2

import java.io.{BufferedWriter, File, FileWriter}
import scala.collection.immutable.ListMap
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.io.Source

class WordCloud {

  def createWordCloud() = {
    var words = loadWords()
    words = removeStopWords(words)
    val countsAndWords = countWords(words)
    saveWordCloud(countsAndWords, 100)

  }

  // const
  val BOOK_FILENAME = "/home/stanislaw/PWR/Big_Data_Anal/2 semester/BDA_lab/list_0/src/main/scala/ex_2/harry_potter_1.txt"
  val STOP_WORDS_FILENAME = "/home/stanislaw/PWR/Big_Data_Anal/2 semester/BDA_lab/list_0/src/main/scala/ex_2/stop_words.txt"
  val OUTPUT_FILENAME = "/home/stanislaw/PWR/Big_Data_Anal/2 semester/BDA_lab/list_0/src/main/scala/ex_2/world_cloud.txt"


  private def loadWords() = {
    val source = Source.fromFile(BOOK_FILENAME)
    val bookLines = try source.getLines.toList finally source.close
    val wordsList = ListBuffer[String]()
    for(line <- bookLines) {
      val split: List[String] = line.split(' ').toList
      for(words <- split) {
        val cleaned = removeRegex(words)
        if (cleaned.nonEmpty) {
          wordsList += words
        }
      }
    }
    wordsList
  }

  private def removeRegex(words: String): String = {
    words.replaceAll("[^A-Za-z0-9']", "")
  }

  private def removeStopWords(wordsList: ListBuffer[String]) = {
    val source = Source.fromFile(STOP_WORDS_FILENAME)
    val stopWordsList =  try source.getLines.toList finally source.close
    val newList = wordsList.filter(!stopWordsList.contains(_))
//    println(newList.length + " " + wordsList.length)
    newList
  }

  private def countWords(wordsList: ListBuffer[String]) = {
    wordsList.groupBy(w => w).map(t => (t._1, t._2.length))
  }

  private def saveWordCloud(wordMap: Map[String, Int], numberOfRows: Int) = {
    val writer = new BufferedWriter(new FileWriter(new File(OUTPUT_FILENAME)))
    val listMapSave = ListMap(wordMap.toSeq.sortWith(_._2 > _._2):_*)
    for  (word <- listMapSave.take(numberOfRows)) {
      println(word._1 + " " + word._2)
      writer write word._1 + " " + word._2
      writer write "\n"
    }
    writer.close()
  }

}
