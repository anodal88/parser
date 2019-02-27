package translator

import java.io._
import scala.io._
import scala.collection.mutable.ListBuffer

class SoyToVueTranslatorService(val templateName: String = "templates/origin/ncl.components.c1.soy") {

  var params = new ListBuffer[String]()
  var dependencies = new ListBuffer[String]()
  var componentDependencies = Map[String, List[String]]()
  val rawText: String = getRawText(templateName)
  var vueTemplate: String = getRawText(templateName)

  val callOpenToken = "{call"
  val genericCloseToken = "}"
  val genericSlashCloseToken = "/}"
  val callCloseToken = "{/call}"
  val separator = "-"
  val paramStartToken = "{@param"

  val ifStartToken = "{if"
  val elseIfToken = "{elseif"
  val elseToken = "{else}"
  val endIfToken = "{/if}"

  val switchToken = "{switch"
  val caseToken = "{case"
  val switchDefaultToken = "{default}"
  val switchCloseToken = "{/switch}"

  val foreachToken = "{foreach"
  val ifEmptyToken = "{ifempty}"
  val foreachCloseToken = "{/foreach}"

  val forToken = "{for"
  val forInToken = "in"
  val endForToken = "{/for}"

  val printStartToken = "{$"
  val printLiteralToken = "{print"

  val spaceToken = "{sp}"
  val emptyStringToken = "{nil}"
  val carriageReturnToken = "{\r}"
  val newLineToken = "{\n}"
  val tabToken = "{\t}"
  val leftBraceToken = "{lb}" //{
  val rightBraceToken = "{rb}" //}
  val literalStartToken = "{literal}"
  val literalEndToken = "{/literal}"

  val declarationToken = "{let"


  /**
    * Removes the namespace keywords
    *
    * @param template
    * @return
    */
  def removeNamespaceTag(tmpl: String): String = {
    tmpl.replaceAll("\\{\\s*namespace\\s*(\\w\\.*)+\\s*\\}", "")
  }

  def removeTemplateTag(tmpl: String) = {
    tmpl.replaceAll("(\\{\\s*template\\s\\.*\\w+\\s*\\})|(\\{\\s*\\/\\s*template\\s*\\})", "")
  }


  def renderIfTemplate(evalExpresion: String, content: String): String = {
    val r =s"""<div v-if= "${this.sanitize(evalExpresion)}">${content}</div>"""
    r
  }

  /**
    * This is the function in charge of if elseif else statements translations
    *
    * @param tmpl
    */
  def translateIfStatements(tmpl: String) = {
    var template = tmpl
    //Extract the if statement sub-template
    //if it is a simple if translate using template from vue
    //if it is a compound if statement wrap content using a simple div

    while (template.contains(this.ifStartToken)) {
      val startIf = template.indexOf(this.ifStartToken)
      val endIfCondition = template.indexOf(this.genericCloseToken, startIf)
      val endIf = template.indexOf(this.endIfToken, startIf)
      val ifBlockContent = template.substring(endIfCondition + 1, endIf)
      val evalExpressions = this.findTokenCollections(template, this.ifStartToken, this.genericCloseToken)
      val hasElseIf = ifBlockContent.contains(this.elseIfToken)
      if (!hasElseIf) {
        //simple if
        val first = template.slice(0, startIf - 1)
        val rest = template.slice(endIf + this.endIfToken.size, template.size)
        val wrapper = this.renderIfTemplate(evalExpressions.head, ifBlockContent)
        template = first + wrapper + rest
        // template = template.slice(0,startIf-1)+renderIfTemplate(evalExpressions.head,ifBlockContent)+template.slice(endIf-1,template.size)
      } else {
        //complex if statement
        //TODO: finish compound cases
      }
    }
    template
  }

  /**
    * Translate all prints from soy to vue interpolation
    * @param tmpl
    * @return
    */
  def translatePrints(tmpl: String): String = {
    this.replacePrints(
      this.replacePrints(
        tmpl, this.printStartToken, this.genericCloseToken
      ), this.printLiteralToken, this.genericCloseToken)
  }

  def replacePrints(tmpl: String, start: String, end: String): String = {
    var res = tmpl
    while (res.contains(start)) {
      val startPos = res.indexOf(start)
      val endPos = res.indexOf(end, startPos)
      if (startPos > 0 && endPos > 0) {
        res = res.slice(0, startPos) + "{{" + res.slice(startPos + start.length, endPos) + "}}" + res.slice(endPos + end.length, res.size)
      }
    }
    res
  }

  /**
    * Removes all soy template self params tags
    *
    * @return
    */
  def removeOwnParamsTags(tmpl: String) = {
    var template = tmpl
    val paramToken = this.paramStartToken
    val paramEndToken = this.genericCloseToken
    while (template.contains(paramToken)) {
      val start = template.indexOf(paramToken)
      val end = template.indexOf(paramEndToken, start)
      if (start > 0 && end > 0) {
        val before = template.slice(0, start - 1)
        val after = template.slice(end + this.genericCloseToken.size, template.size)
        template = before + after
      }
    }
    template
  }

  /**
    * Replaces all call to other soy templates with the similar call on vue js
    */
  def replaceCallsRenderingVueComponents(tmpl: String) = {
    var result = tmpl
    val componentDependencyMap: List[(String, Map[String, String])] = this.getVueComponentDependencyMap(tmpl)
    for ((cmp, params) <- componentDependencyMap) {
      val cmpSelector = cmp.replaceAll(" ", "").replace(".", this.separator)
      val renderedCmpBody = this.renderVueComponent(cmpSelector, params)
      val cmpCallIndex = result.indexOf(this.callOpenToken)
      val cmpCallCloseIndex = result.indexOf(this.callCloseToken, cmpCallIndex)
      result = result.slice(0, cmpCallIndex - 1) + renderedCmpBody + result.slice(cmpCallCloseIndex + this.callCloseToken.size, result.size)

    }
    result
  }

  def lintContent(content: String): String = {
    ""
  }

  /**
    * Create a filename with the component
    *
    * @param component_selector
    * @param props
    * @param template
    */
  def writeComponent() = {
    val cmpName = (this.getTemplateNamesPace() + "-" + this.getTemplateName())
      .replace(".", "-")
      .replace(" ", "")
    val cmpTemplate = this.getVueTemplate()
    val cmpProps = this.getParams()

    val stringProps = cmpProps.map(e => s"'$e'").mkString(" , ")
    val body = getRawText("templates/origin/vue_template.js")
      .replace("#!PROPS!#", stringProps)
      .replace("#!SELECTOR!#", s"'${cmpName}'")
      .replace("#!TEMPLATE!#", s"`${cmpTemplate}`")

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
  def findDependencies(template: String) = {
    this.findTokenCollections(template, "{call", "}")
  }

  /**
    * Return a list of Tuple containing each component and it's input props
    *
    * @return
    */
  def getVueComponentDependencyMap(tmpl: String) = {
    var result: List[(String, Map[String, String])] = List()
    val components = this.findTokenCollections(tmpl, "{call", "}")
    for (cmp <- components) {
      var paramsMap = Map[String, String]()
      val subCmpStart = this.rawText.indexOf(cmp)
      val subCmpEnd = this.rawText.indexOf("{/call}", subCmpStart)
      val subTemplate = this.rawText.slice(subCmpStart, subCmpEnd)
      paramsMap = this.findTokenCollections(subTemplate, "{param", "/}", true)
        .filter(p => p.contains(":"))
        .map(p => {
          val parts = p.split(":")
          (parts.head -> parts.tail.head.replaceAll("\\$", ""))
        }).toMap
      result = result :+ (cmp, paramsMap)
    }
    result
  }

  /**
    * Return a list of all props required by the main component
    *
    * @return
    */
  def findParams(template: String): List[String] = {
    this.findTokenCollections(template, "@param", ":", true)
  }

  /**
    * Returns a collection of the given tokenNameStart and tokenNameEnd pattern in the soy template
    *
    * @param paramNameStart
    * @param paramNameEnd
    * @return
    */
  def findTokenCollections(template: String, paramNameStart: String, paramNameEnd: String, sanitizeValue: Boolean = false): List[String] = {
    var pos = 0
    var result = new ListBuffer[String]()
    while (pos < template.size) {
      val paramNameStartPos = template.indexOf(paramNameStart, pos)
      val paramNameEndPos = template.indexOf(paramNameEnd, paramNameStartPos)
      if (paramNameStartPos > 0 && paramNameEndPos > 0) {
        val testValue = template.slice(paramNameStartPos + paramNameStart.size, paramNameEndPos)
        result += {
          if (sanitizeValue) this.sanitize(testValue) else testValue
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
    str.replaceAll("\\.|_|\\*| |\\?|\\$", "")
  }

  /**
    * Returns a valid vue template
    *
    * @return
    */
  def getVueTemplate(): String = {
    val template = this.replaceCallsRenderingVueComponents(
      this.removeNamespaceTag(
        this.removeTemplateTag(
          this.removeOwnParamsTags(
            this.translateIfStatements(
              this.translatePrints(this.rawText)
            )
          )
        )
      )
    )
    template
  }

  /**
    * Render vue component using the given props
    *
    * @param selector
    * @param props
    * @return
    */
  def renderVueComponent(selector: String, props: Map[String, String]) = {
    var result: String = ""
    val componentOpenTag = s"<${selector} \n"
    val componentOpenTagClose = ">"
    val componentCloseTag = s"</${selector}>"
    result += componentOpenTag
    for ((prop, value) <- props) {
      result += s"\tv-bind:${prop} = ${value}\n"
    }
    result += componentOpenTagClose + componentCloseTag
    result
  }

}
