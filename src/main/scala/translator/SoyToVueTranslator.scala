package translator

import java.io._

import scala.io._
import scala.collection.mutable.ListBuffer
// TODO: identify required from optional parameters
// TODO: finish special tags translations
// TODO: finish switch tag translation
// TODO: finish var declaration translation

class SoyToVueTranslator(val templateName: String = "templates/origin/ncl.components.c1.soy") {

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

  val vueInterpolationStartToken = "{{"
  val vueInterpolationEndToken = "}}"

  /** ******** VUE DIRECTIVES *************/
  val vIf = "v-if"
  val vElseIf = "v-else-if"
  val vElse = "v-else"
  val vTemplateStart = "<template"
  val vTemplateEnd = "</template>"

  val fnTemplate = ""
  val vueCmpSelectorTmplToken = "{SELECTOR}"
  val vueCmpDataTmplToken = "{DATA}"
  val vueCmpPropsTmplToken = "{PROPS}"
  val vueCmpTemplateTmplToken = "{TEMPLATE}"
  val vueCmpComputedTmplToken = "{COMPUTED}"
  val vueComponentTemplate = s"Vue.component(${vueCmpSelectorTmplToken}, {data: {${vueCmpDataTmplToken}},props:[${vueCmpPropsTmplToken}],computed:{${vueCmpComputedTmplToken}},template: ${vueCmpTemplateTmplToken})"

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


  def renderIfTemplate(evalExpresion: String, vueDirective: String, content: String): String = {
    val start: String = s"""<template ${vueDirective}"""
    val end: String = s""">${content}</template>"""
    val expresion: String = if (evalExpresion.size > 0)s""" = "${this.sanitize(evalExpresion)}"""" else ""
    start + expresion + end
  }


  /**
    * Translate if statements block by block starting from inside to outside
    *
    * @param tmpl
    * @return
    */
  def translateIfStatements(tmpl: String): String = {
    var template = tmpl
    while (template.contains(this.ifStartToken)) {
      val start = template.lastIndexOf(this.ifStartToken)
      //start from inside to outside
      val end = template.indexOf(this.endIfToken, start)
      var block = template.slice(start, end + this.endIfToken.size)
      val wrapperBuffer = new ListBuffer[String]()
      val posIfEnd = {
        if (block.indexOf(this.elseIfToken) >= 0)
          block.indexOf(this.elseIfToken)
        else if (block.indexOf(this.elseToken) >= 0)
          block.indexOf(this.elseToken)
        else if (block.indexOf(this.endIfToken) >= 0)
          block.indexOf(this.endIfToken)
        else
          end
      }
      val ifBlock = template.slice(start, end)
      if (ifBlock.size > 0) {
        val simpleClosePos = block.indexOf(this.genericCloseToken)
        val blockContent = block.substring(simpleClosePos + 1, posIfEnd)
        val evalExpression = this.findTokenCollections(block, this.ifStartToken, this.genericCloseToken)
          .head.replace(" ", "")
        val ifVueWrapper = this.renderIfTemplate(evalExpression, this.getVueIfDirective(this.ifStartToken), blockContent)
        wrapperBuffer += ifVueWrapper
      }

      while (block.contains(this.elseIfToken)) {
        val elseIfCloseBlock = "{else"
        val startEif = block.indexOf(this.elseIfToken)
        val endEif = block.indexOf(elseIfCloseBlock, startEif + 1)
        val elseIfContent = block.slice(block.indexOf(this.genericCloseToken, startEif + 1) + this.genericCloseToken.size, endEif)
        val elseIfEvalExpr = this.findTokenCollections(block, this.elseIfToken, this.genericCloseToken).head.replace(" ", "")
        val elseIfVueWrapper = this.renderIfTemplate(elseIfEvalExpr, this.getVueIfDirective(this.elseIfToken), elseIfContent)
        wrapperBuffer += elseIfVueWrapper
        val head = block.slice(0, startEif)
        val tail = block.slice(endEif, block.size)
        block = head + tail
      }

      if (block.contains(this.elseToken)) {
        val elseStart = block.indexOf(this.elseToken)
        val elseBlockContent = block.slice(elseStart + this.elseToken.size, block.indexOf(this.endIfToken))
        val elseWrapper = this.renderIfTemplate("", this.getVueIfDirective(this.elseToken), elseBlockContent)
        wrapperBuffer += elseWrapper
      }
      val newHeadBlock = template.slice(0, start)
      val newTailBlock = template.slice(end + this.endIfToken.size, template.size)
      template = newHeadBlock + wrapperBuffer.mkString("\n") + newTailBlock
    }
    template
  }


  /**
    *
    * @param soyIfTag
    * @return
    */
  private def getVueIfDirective(soyIfTag: String): String = {
    soyIfTag match {
      case this.ifStartToken => this.vIf
      case this.elseIfToken => this.vElseIf
      case this.elseToken => this.vElse
      case _ => throw new Exception("Invalid Soy If tag")
    }
  }


  /**
    * Convert literals to vue
    *
    * @param tmpl
    * @return
    */
  def translateLiterals(tmpl: String) = {
    tmpl.replace(this.literalStartToken, "").replace(this.literalEndToken, "")
  }

  def getIfEmptyWrapper(collection: String, content: String): String = {
    s"""${this.vTemplateStart} v-show="!(${collection})">${content}${this.vTemplateEnd}"""
  }

  def getVueForWrapper(evalExpr: String, content: String): String = {
    s"""${this.vTemplateStart} v-for="${evalExpr}">${content}${this.vTemplateEnd}"""
  }

  /**
    * Translate Loop structures from Closure to Vue
    * @param tmpl
    * @return
    */
  def translateForStatements(tmpl: String): String = {

    def loopTransHelper(source: String, startTag: String, endTag: String): String = {
      var template = source
      while (template.contains(startTag)) {
        val forStart = template.lastIndexOf(startTag)
        val forEnd = template.indexOf(endTag, forStart)
        val forBlock = template.slice(forStart, forEnd + endTag.size)
        var ifEmptyWrapper = ""
        var forBlockContent = forBlock.slice(forBlock.indexOf(this.genericCloseToken) + this.genericCloseToken.size, forBlock.size - endTag.size)
        val forEvalExpr = this.sanitize(this.findTokenCollections(forBlock, startTag, this.genericCloseToken).head, false)
        if (forBlock.contains(this.ifEmptyToken)) {
          //Extract this part from the forBlock b/c vue do not support it and expose it as a separate condition
          val ifEmptyStartPos = forBlock.indexOf(this.ifEmptyToken)
          val ifEmptyContent = forBlock.slice(ifEmptyStartPos + this.ifEmptyToken.size, forBlock.size - endTag.size)
          val loopCollection = forEvalExpr.slice(forEvalExpr.indexOf(this.forInToken) + this.forInToken.size, forEvalExpr.size)
          ifEmptyWrapper = this.getIfEmptyWrapper(loopCollection, ifEmptyContent)
          forBlockContent = forBlockContent.slice(0,forBlockContent.indexOf(this.ifEmptyToken))
        }
        val vueForWrapper = this.getVueForWrapper(forEvalExpr, forBlockContent)
        val head = template.slice(0, forStart)
        val tail = template.slice(forEnd + endTag.size, template.size)
        template = head + vueForWrapper + {
          if (ifEmptyWrapper.size > 0) ifEmptyWrapper else ""
        } + tail
      }
      template
    }

   // loopTransHelper(
      loopTransHelper(tmpl, this.foreachToken, this.foreachCloseToken)//,
    //  this.forToken, this.endForToken
   // )
  }

  /**
    * Translate all prints from soy to vue interpolation
    *
    * @param tmpl
    * @return
    */
  def translatePrints(tmpl: String): String = {

    def replaceOpenCloseTags(tmpl: String, start: String, end: String, startReplace: String, endReplace: String): String = {
      var res = tmpl
      while (res.contains(start)) {
        val startPos = res.indexOf(start)
        val endPos = res.indexOf(end, startPos)
        if (startPos > 0 && endPos > 0) {
          res = res.slice(0, startPos) + startReplace + res.slice(startPos + start.length, endPos) + endReplace + res.slice(endPos + end.length, res.size)
        }
      }
      res
    }

    val vueInterpolationStart = "{{"
    val vueInterpolationEnd = "}}"
    replaceOpenCloseTags(
      replaceOpenCloseTags(
        tmpl, this.printStartToken, this.genericCloseToken, vueInterpolationStart, vueInterpolationEnd
      ), this.printLiteralToken, this.genericCloseToken, vueInterpolationStart, vueInterpolationEnd)
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

  /**
    * Lint the given content
    *
    * @param content
    * @return
    */
  def lintContent(content: String): String = {
    content.replaceAll("</", "\n</")
      .replaceAll(">", ">\n")
      .replaceAll("\n\n|\n\r", "\n")
  }

  /**
    * Export the component
    *
    * @param singleFile  wether if the component will be in one file or in folder containing 3 files (js, scss, html)
    * @param destination is the destination path where the component will be saved
    */
  def exportVueComponent(singleFile: Boolean = true, destination: String = "") = {
    val cmpName = (this.getTemplateNamesPace() + "-" + this.getTemplateName())
      .replace(".", "-")
      .replace(" ", "")
    val cmpTemplate = this.getVueTemplate()
    val cmpProps = this.getParams()
    val stringComputed = "" // TODO: calculate the computed functions
    val stringData = "" // TODO: Calculate data
    val stringProps = cmpProps.map(e => s"'$e'").mkString(" , ")
    val body = this.vueComponentTemplate
      .replace(this.vueCmpPropsTmplToken, stringProps)
      .replace(this.vueCmpSelectorTmplToken, s"'${cmpName}'")
      .replace(this.vueCmpTemplateTmplToken, s"`${cmpTemplate}`")
      .replace(this.vueCmpComputedTmplToken, s"${stringComputed}")
      .replace(this.vueCmpDataTmplToken, s"${stringData}")

    val writer = new PrintWriter(new File(s"${cmpName}.js"))
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
      if (paramNameStartPos >= 0 && paramNameEndPos >= 0) {
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
  def sanitize(str: String, removesSpaces: Boolean = true): String = {
    val exp = "\\.|_|\\*" + {
      if (removesSpaces) "| " else ""
    } + "|\\?|\\$"
    str.replaceAll(exp, "")
  }

  /**
    * Returns a valid vue template
    *
    * @return
    */
  def getVueTemplate(): String = {
    val template =
      this.lintContent(
        this.translateIfStatements(
          this.translateLiterals(
            this.translateForStatements(
              this.translatePrints(
                this.replaceCallsRenderingVueComponents(
                  this.removeNamespaceTag(
                    this.removeTemplateTag(
                      this.removeOwnParamsTags(
                        this.rawText
                      )
                    )
                  )
                )
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
