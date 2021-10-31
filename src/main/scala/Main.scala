import ex_1.HelloWorld
import ex_2.WordCloud
import ex_3.WordCloudGenerator

object Main {
  def main(args: Array[String]): Unit = {
    val ex1 = new HelloWorld
    ex1.sayHello()
    val ex2 = new WordCloud
    ex2.createWordCloud()
    val ex3 = new WordCloudGenerator
    ex3.runGenerator()
  }

}
