package translator

import java.io._

import scala.io._
import scala.util.matching.Regex
import scala.collection.mutable.ListBuffer

class SoyToVueTranslatorService(val templateName: String = "templates/origin/ncl.components.c1.soy") {

  var params = new ListBuffer[String]()
  var dependencies = new ListBuffer[String]()
  var componentDependencies = Map[String, List[String]]()
  val rawText: String = getRawText(templateName)

  /**
    * Removes the namespace keywords
    *
    * @param template
    * @return
    */
  def removeNamespace(template: String = ""): String = {
    getRawText(template).replaceAll("\\{\\s*namespace\\s*(\\w\\.*)+\\s*\\}", "")
  }

  /**
    * Create a filename with the component
    *
    * @param component_selector
    * @param props
    * @param template
    */
  def writeComponent(component_selector: String, props: Array[String] = Array("name", "lastname"), template: String = "<b>Test</b>") = {
    val stringProps = props.map(e => s"'$e'").mkString(" , ")
    val body = getRawText("templates/origin/vue_template.js")
      .replace("#!PROPS!#", stringProps)
      .replace("#!SELECTOR!#", s"'${component_selector}'")
      .replace("#!TEMPLATE!#", s"`${template}`")

    val writer = new PrintWriter(new File("example.js"))
    writer.write(body)
    writer.close()
  }


  /**
    * Get all words of the template
    *
    * @return
    */
  def getRawText(path: String): String = {
    var rawText: StringBuilder = new StringBuilder()
    try {
      for (line <- Source.fromResource(path).getLines()) {
        rawText.append(line.toString)
      }
    } catch {
      case e: FileNotFoundException => println("Couldn't find that file.")
      case e: IOException => println("Got an IOException!")
      case e: Exception => println("Unknown Exception")
    }
    rawText.toString()
  }

  /**
    * Returns the params defined in the soy template
    * //TODO: differentiate optionals from required
    *
    * @param optional
    * @return
    */
  def getParams(): List[String] = {
    var params = new ListBuffer[String]()
    var template = getRawText(this.templateName)
    val param = "@param"
    var pos = 0
    while (template.contains(param)) {
      val paramStart = template.indexOf(param, pos)
      val paramEnd = template.indexOf(":", paramStart)
      val paramName = template.slice(paramStart + param.size, paramEnd)
      template = template.slice(paramEnd, template.size)
      pos = paramEnd + 1
      params += paramName.replaceAll(" ", "").replace("?", "")
    }
    params.toList
  }

  def getTemplateName(): String = {
    getTokenValue("template", "}", true)
  }

  /**
    * Find the value of a token that hold the given start and end identifiers
    *
    * @param startToken
    * @param endToken
    * @return
    */
  def getTokenValue(startToken: String, endToken: String, sanitizeValue: Boolean = false): String = {
    val tmplStart = this.rawText.indexOf(startToken) + startToken.size
    val tmplEnd = this.rawText.indexOf(endToken, tmplStart)
    val preResult = this.rawText.slice(tmplStart, tmplEnd)
    if (sanitizeValue) sanitize(preResult) else preResult
  }

  /**
    * Returns the namespace of the soy template
    *
    * @return
    */
  def getTemplateNamesPace(): String = {
    getTokenValue("namespace", "}")
  }

  /**
    * Fill the dependencies local parameter
    */
  def findDependencies() = {
    this.findTokenCollections("{call", "}")
  }

  /**
    * Return a list of Tuple containing each component and it's input props
    *
    * @return
    */
  def getVueComponentDependencyMap() = {
    var result : List[(String,List[String])] = List()
    val components = this.findTokenCollections("{call", "}")
    for (cmp <- components) {
      val subCmpStart = this.rawText.indexOf(cmp)
      val subCmpEnd = this.rawText.indexOf("{/call}", subCmpStart)
      val subTemplate = this.rawText.slice(subCmpStart, subCmpEnd)
      result = result:+(cmp, this.findTokenCollections("{param", ":",true,subTemplate))
    }
    result
  }

  /**
    * Return a list of all props required by the main component
    *
    * @return
    */
  def findParams(): List[String] = {
    this.findTokenCollections("@param", ":", true)
  }

  /**
    * Returns a collection of the given tokenNameStart and tokenNameEnd pattern in the soy template
    *
    * @param paramNameStart
    * @param paramNameEnd
    * @return
    */
  def findTokenCollections(paramNameStart: String, paramNameEnd: String, sanitizeValue: Boolean = false, tmpl: String = ""): List[String] = {
    var pos = 0
    val template = if(tmpl.size > 0) tmpl else this.rawText
    var result = new ListBuffer[String]()
    while (pos < template.size) {
      val paramNameStartPos = template.indexOf(paramNameStart, pos)
      val paramNameEndPos = template.indexOf(paramNameEnd, paramNameStartPos)
      if (paramNameStartPos > 0 && paramNameEndPos > 0) {
        val testValue = template.slice(paramNameStartPos + paramNameStart.size, paramNameEndPos)
        if (sanitizeValue) {
          result += this.sanitize(testValue)
        } else {
          result += testValue
        }
      }
      pos = if (paramNameStartPos > 0) paramNameEndPos + 1 else template.size
    }
    result.toList

  }

  /**
    * Removes weird characters from the given string
    *
    * @param str
    * @return
    */
  def sanitize(str: String): String = {
    str.replaceAll("\\.|_|\\*| |\\?", "")
  }

  def getVueTemplate(): String = {
    ""
  }
}
