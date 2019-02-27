package translator

import scala.io.Source
import java.io.{FileNotFoundException, IOException}
import java.io.PrintWriter
import java.io.File

object Translator {

  val translatorService = new SoyToVueTranslatorService()


  def writeFile(msg: String) = {
    val writer = new PrintWriter(new File("Write.txt"))

    writer.write("Hello Developer, Welcome to Scala Programming.")
    writer.close()
  }

  def sayHello() = {
    println("hello")
  }

  def main(args: Array[String]): Unit = {

    //translatorService.writeComponent("my-custom-component")
    //println(translatorService.getTemplateName())
    //println(translatorService.findParams())
    //println(translatorService.getTemplateNamesPace())
    //println(translatorService.findDependencies())
    //println(translatorService.getVueComponentDependencyMap())
    println(translatorService.renderVueComponent("my-cmp",List("name","Lastname")))
  }
}
