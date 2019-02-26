package translator

import scala.io.Source
import java.io.{FileNotFoundException, IOException}
import java.io.PrintWriter
import java.io.File

object Translator {

  val parserService = new SoyToVueParserService()
  val originFile = "example.soy"

  def translateFromSoyTemplate(soyTemplate: String) = {

  }

  def get_file_contents(path: String): Unit = {
    var content: Array[String] = Array()
    try {
      for (line <- Source.fromResource("templates/origin/example.soy").getLines()) {
       println(line.split(" +"))
      }

    } catch {
      case e: FileNotFoundException => println("Couldn't find that file.")
      case e: IOException => println("Got an IOException!")
    }
   // content
  }

  def writeFile(msg: String) = {
    val writer = new PrintWriter(new File("Write.txt"))

    writer.write("Hello Developer, Welcome to Scala Programming.")
    writer.close()
  }

  def sayHello() = {
    println("hello")
  }

  def main(args: Array[String]): Unit = {
    get_file_contents(originFile)

  }
}
