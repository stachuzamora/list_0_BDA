package ex_3


import java.io.{BufferedWriter, File, FileNotFoundException, FileWriter}
import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.sys.exit


class WordCloudGenerator {

  var WORD_CLOUD_MAP = mutable.Map[String, Int]()

  def runGenerator(): Unit = {
    while (true) {
      showWelcomeMessage()
      if (getInputAndProcess) {
        println("Thank you! Bye")
        exit(0)
      }
    }
  }

  private def showWelcomeMessage(): Unit = {
    println("Welcome to the Word Cloud Generator! Choose one option: ")
    println("1 - input your sentence")
    println("2 - input file path")
    println("3 - show World Cloud in the console")
    println("4 - save World Cloud to the file")
    println("5 - exit")
  }


  private def getInputAndProcess: Boolean = {
    try {
      val input = scala.io.StdIn.readLine().toInt
      input match {
        case 1 => readStringSentence
        case 2 => readFromFile
        case 3 => showWordCloud
        case 4 => saveWordCloud
        case 5 => true
        case _ =>
          println("Choose one of given options")
          false
      }
    } catch {
      case _: NumberFormatException =>
        println("Input number, not string")
        false
    }
  }

  private def readStringSentence: Boolean = {
    println("Provide the sentence: ")
    val input = scala.io.StdIn.readLine()
    var output = readWords(input)
    output = removeStopWords(output)
    val outputMap = countWords(output)
    outputMap.foreach(p => println(p._1 + " " + p._2))
    outputMap.foreach(p => {
      WORD_CLOUD_MAP.updateWith(p._1) {
        case Some(value) => Some(value + p._2)
        case None => Some(p._2)
      }
    })
    WORD_CLOUD_MAP = mutable.Map(WORD_CLOUD_MAP.toSeq.sortWith(_._2 > _._2):_*)
    //    WORD_CLOUD_MAP ++ outputMap.map { case (k, v) => k -> (v + outputMap.getOrElse(k, 0)) }
    false
  }


  private def readFromFile = {
    println("Provide file to read words from: ")
    val input = scala.io.StdIn.readLine()
    try {
      val source = Source.fromFile(input)
      val bookLines = try source.getLines.toList finally source.close
      var wordsList = ListBuffer[String]()
      for (line <- bookLines) {
        val split: List[String] = line.split(' ').toList
        for (words <- split) {
          val cleaned = words.replaceAll("[^A-Za-z0-9']", "")
          if (cleaned.nonEmpty) {
            wordsList += words
          }
        }
      }
      wordsList = removeStopWords(wordsList)
      val countWordsMap = countWords(wordsList)
      countWordsMap.foreach(p => println(p._1 + " " + p._2))
//      WORD_CLOUD_MAP ++ countWordsMap.map { case (k, v) => k -> (v + countWordsMap.getOrElse(k, 0)) }
      countWordsMap.foreach(p => {
        WORD_CLOUD_MAP.updateWith(p._1) {
          case Some(value) => Some(value + p._2)
          case None => Some(p._2)
        }
      })
      WORD_CLOUD_MAP = mutable.Map(WORD_CLOUD_MAP.toSeq.sortWith(_._2 > _._2):_*)
    } catch {
      case _: FileNotFoundException => println("No such file, provide correct one")
    }

      false
    }

    private def showWordCloud = {
      println("*************MAP***********")
      WORD_CLOUD_MAP.foreach(p => println(p._1 + " " + p._2))
      println("***************************")
      false
    }

    private def saveWordCloud = {
      println("Provide file name to save the current Word Cloud: ")
      val fileName = scala.io.StdIn.readLine()
      val writer = new BufferedWriter(new FileWriter(new File(fileName)))
      val listMapSave = ListMap(WORD_CLOUD_MAP.toSeq.sortWith(_._2 > _._2):_*)
      for  (word <- listMapSave) {
        println(word._1 + " " + word._2)
        writer write word._1 + " " + word._2
        writer write "\n"
      }
      writer.close()
      false
    }

    private def readWords(sentence: String) = {
      val words = sentence.split(' ').toList
      val wordsList = ListBuffer[String]()
      for (line <- words) {
        val split: List[String] = line.split(' ').toList
        for (words <- split) {
          val cleaned = words.replaceAll("[^A-Za-z0-9']", "")
          if (cleaned.nonEmpty) {
            wordsList += words
          }
        }
      }
      wordsList
    }

    private def removeStopWords(wordsList: ListBuffer[String]) = {
      val STOP_WORDS_FILENAME = "/home/stanislaw/PWR/Big_Data_Anal/2 semester/BDA_lab/list_0/src/main/scala/ex_2/stop_words.txt"
      val source = Source.fromFile(STOP_WORDS_FILENAME)
      val stopWordsList = try source.getLines.toList finally source.close
      val newList = wordsList.filter(!stopWordsList.contains(_))
      newList
    }

    private def countWords(wordsList: ListBuffer[String]) = {
      wordsList.groupBy(w => w).map(t => (t._1, t._2.length))
    }

  }
