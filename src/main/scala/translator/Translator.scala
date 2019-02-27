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

    translatorService.writeComponent()
    //println(translatorService.getTemplateName())
    //println(translatorService.findParams())
    //println(translatorService.getTemplateNamesPace())
    //println(translatorService.findDependencies())
    //println(translatorService.getVueComponentDependencyMap())
    //println(translatorService.renderVueComponent("my-cmp",Map("duration" -> "$duration", "summaryList" -> "$summaryList", "favoriteStatus" -> "$favoriteStatus")))
    //println(translatorService.removeNamespaceTag())
    //println(translatorService.removeTemplateTag())
    //println(translatorService.removeOwnParamsTags())
    //translatorService.replaceCallsRenderingVueComponents()
    //println(translatorService.getVueTemplate())
    //println(translatorService.translateIfStatements(translatorService.rawText))
  }
}
