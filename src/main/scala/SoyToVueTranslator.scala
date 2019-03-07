object SoyToVueTranslator {

  import java.io._
  import scala.io._

  class SoyToVueTranslator(filename: String) {

    val rawText: String = getRawText(filename)

    val callOpenToken = "{call"
    val genericCloseToken = "}"
    val genericSlashCloseToken = "/}"
    val callCloseToken = "{/call}"
    val separator = "-"
    val paramStartToken = "{@param"
    val paramStartInCallToken = "{param"
    val paramCloseToken="{/param}"
    val colonToken = ":"

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

    /** * CLOSURE SPECIAL TAGS **/
    val spaceToken = "\\{sp\\}"
    val emptyStringToken = "\\{nil\\}"
    val carriageReturnToken = "\\{\r\\}"
    val newLineToken = "\\{\n\\}"
    val tabToken = "\\{\t\\}"
    val leftBraceToken = "\\{lb\\}"
    val rightBraceToken = "\\{rb\\}"

    val literalStartToken = "{literal}"
    val literalEndToken = "{/literal}"

    val declarationToken = "{let"
    val declarationEndToken = "{/let}"
    val logicalAnd = " and "
    val logicalNegation = " not "
    val logicalOr = " or "

    val vueInterpolationStartToken = "{{"
    val vueInterpolationEndToken = "}}"

    /** ******** VUE DIRECTIVES *************/
    val vIf = "v-if"
    val vElseIf = "v-else-if"
    val vElse = "v-else"
    val vTemplateStart = "<template"
    val vTemplateEnd = "</template>"
    val vAnd = " && "
    val vOr = " || "
    val vNegation = " ! "

    val fnTemplate = ""
    val vueCmpSelectorTmplToken = "{SELECTOR}"
    val vueCmpDataTmplToken = "{DATA}"
    val vueCmpPropsTmplToken = "{PROPS}"
    val vueCmpTemplateTmplToken = "{TEMPLATE}"
    val vueCmpComputedTmplToken = "{COMPUTED}"
    val vueComponentTemplate = s"Vue.component(${vueCmpSelectorTmplToken}, {data: {${vueCmpDataTmplToken}},props:{${vueCmpPropsTmplToken}},computed:{\n${vueCmpComputedTmplToken}},template: ${vueCmpTemplateTmplToken})"

    /**
      * Removes the namespace keywords
      *
      * @param template
      * @return
      */
    def removeNamespaceTag(tmpl: String): String = {
      tmpl.replaceAll("\\{\\s*namespace\\s*(\\w\\.*)+\\s*\\}", "")
    }

    /**
      * Removes the Closure template tag in order to start clean
      *
      * @param tmpl String template
      * @return
      */
    def removeTemplateTag(tmpl: String) = {
      tmpl.replaceAll("(\\{\\s*template\\s\\.*\\w+\\s*\\})|(\\{\\s*\\/\\s*template\\s*\\})", "")
    }

    /**
      * Helper function used as template to render vue conditional directives
      *
      * @param evalExpresion
      * @param vueDirective
      * @param content
      * @return
      */
    def renderIfTemplate(evalExpresion: String, vueDirective: String, content: String): String = {
      val start: String = s"""<template ${vueDirective}"""
      val end: String = s""">${content}</template>"""
      val expresion: String = if (evalExpresion.size > 0)s""" = "${this.sanitize(evalExpresion, false)}"""" else ""
      start + expresion + end
    }

    /**
      * Translate if statements block by block starting from inside to outside
      *
      * @param tmpl String template
      * @return
      */
    def translateIfStatements(tmpl: String): String = {
      /**
        * Get the vue wrappers for all `else if` cases
        *
        * @param source
        * @return
        */
      def getElseIfBlockWrappers(source: String): String = {
        if (!source.contains(this.elseIfToken)) {
          ""
        } else {
          val elseIfCloseBlock = "{else"
          val startEif = source.indexOf(this.elseIfToken)
          val endEifByElseOrElseIf = source.indexOf(elseIfCloseBlock, startEif + 1)
          val endEifByEndIf = source.indexOf(this.endIfToken, startEif + 1)
          val endEif = List(endEifByElseOrElseIf, endEifByEndIf).filter(_ >= 0).min
          val elseIfContent = source.slice(source.indexOf(this.genericCloseToken, startEif + 1) + this.genericCloseToken.size, endEif)
          val elseIfEvalExpr = this.getContentBtwTokens(source, this.elseIfToken, this.genericCloseToken).head.replace(" ", "")
          val elseIfVueWrapper = this.renderIfTemplate(elseIfEvalExpr, this.getVueIfDirective(this.elseIfToken), elseIfContent)

          val head = source.slice(0, startEif)
          val tail = source.slice(endEif, source.size)
          elseIfVueWrapper + getElseIfBlockWrappers(head + tail)
        }
      }

      /**
        * Get the vue wrapper for `else` case
        *
        * @param source
        * @return
        */
      def getElseBlockWrapper(source: String): String = {
        if (source.contains(this.elseToken)) {
          val elseStart = source.indexOf(this.elseToken)
          val elseBlockContent = source.slice(elseStart + this.elseToken.size, source.indexOf(this.endIfToken))
          val elseWrapper = this.renderIfTemplate("", this.getVueIfDirective(this.elseToken), elseBlockContent)
          elseWrapper
        } else {
          ""
        }
      }

      if (!tmpl.contains(this.ifStartToken)) {
        tmpl
      } else {
        val start = tmpl.lastIndexOf(this.ifStartToken)
        val end = tmpl.indexOf(this.endIfToken, start)
        val block = tmpl.slice(start, end + this.endIfToken.size)
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
        val simpleClosePos = block.indexOf(this.genericCloseToken)
        val blockContent = block.substring(simpleClosePos + 1, posIfEnd)
        val evalExpression = this.getContentBtwTokens(block, this.ifStartToken, this.genericCloseToken)
          .head
        val ifVueWrapper = this.renderIfTemplate(evalExpression, this.getVueIfDirective(this.ifStartToken), blockContent)
        val elseIfVueWrappers = getElseIfBlockWrappers(block)
        val elseVueWrapper = getElseBlockWrapper(block)
        val newHeadBlock = tmpl.slice(0, start)
        val newTailBlock = tmpl.slice(end + this.endIfToken.size, tmpl.size)
        val newTmpl = newHeadBlock + ifVueWrapper + elseIfVueWrappers + elseVueWrapper + newTailBlock
        translateIfStatements(newTmpl)
      }
    }

    /**
      * Return v-if | v-else-if | v-else based on @soyIfTag
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
      * @param tmpl String template
      * @return
      */
    def translateLiterals(tmpl: String) = {
      tmpl.replace(this.literalStartToken, "").replace(this.literalEndToken, "")
    }

    def getVueForWrapper(evalExpr: String, content: String): String = {
      s"""${this.vTemplateStart} v-for="${evalExpr}">${content}${this.vTemplateEnd}"""
    }

    /**
      * Helper function to translate `for` and `foreach`
      *
      * @param source
      * @param startTag
      * @param endTag
      * @return
      */
    private def loopTransHelper(source: String, startTag: String, endTag: String): String = {

      def getIfEmptyWrapper(loopBlock: String, loopEvalExpr: String): String = {
        if (!loopBlock.contains(this.ifEmptyToken)) {
          ""
        } else {
          val ifEmptyStartPos = loopBlock.indexOf(this.ifEmptyToken)
          val ifEmptyContent = loopBlock.slice(ifEmptyStartPos + this.ifEmptyToken.size, loopBlock.size - endTag.size)
          val loopCollection = loopEvalExpr.slice(loopEvalExpr.indexOf(this.forInToken) + this.forInToken.size, loopEvalExpr.size)
          s"""${this.vTemplateStart} v-show="!(${loopCollection})">${ifEmptyContent}${this.vTemplateEnd}"""
        }
      }

      if (!source.contains(startTag)) {
        source
      } else {
        val forStart = source.lastIndexOf(startTag)
        val forEnd = source.indexOf(endTag, forStart)
        val forBlock = source.slice(forStart, forEnd + endTag.size)

        val forBlockContent = forBlock.slice(forBlock.indexOf(this.genericCloseToken) + this.genericCloseToken.size, forBlock.size - endTag.size)
        val forBlockContentWithoutIfEmpty = {
          if (forBlockContent.contains(this.ifEmptyToken))
            forBlockContent.slice(0, forBlockContent.indexOf(this.ifEmptyToken) - 1)
          else
            forBlockContent
        }
        val forEvalExpr = this.sanitize(this.getContentBtwTokens(forBlock, startTag, this.genericCloseToken).head, false)
        val ifEmptyWrapper = getIfEmptyWrapper(forBlock, forEvalExpr)
        val vueForWrapper = this.getVueForWrapper(forEvalExpr, forBlockContentWithoutIfEmpty)
        val head = source.slice(0, forStart)
        val tail = source.slice(forEnd + endTag.size, source.size)
        val newSource = head + vueForWrapper + {
          if (ifEmptyWrapper.size > 0) ifEmptyWrapper else ""
        } + tail
        loopTransHelper(newSource, startTag, endTag)
      }
    }

    /**
      * Translate Loop structures from Closure to Vue
      *
      * @param tmpl String template
      * @return
      */
    private def translateForStatements(tmpl: String): String = {
      loopTransHelper(
        loopTransHelper(tmpl, this.foreachToken, this.foreachCloseToken),
        this.forToken,
        this.endForToken
      )
    }

    /**
      * Translate all prints from soy to vue interpolation
      *
      * @param tmpl String template
      * @return
      */
    def translatePrints(tmpl: String): String = {

      def replaceOpenCloseTags(tmpl: String, start: String, end: String, startReplace: String, endReplace: String): String = {
        if (!tmpl.contains(start)) {
          tmpl
        } else {
          val startPos = tmpl.indexOf(start)
          val endPos = tmpl.indexOf(end, startPos)
          val newTmpl = tmpl.slice(0, startPos) + startReplace + tmpl.slice(startPos + start.length, endPos) + endReplace + tmpl.slice(endPos + end.length, tmpl.size)
          replaceOpenCloseTags(newTmpl, start, end, startReplace, endReplace)
        }
      }

      replaceOpenCloseTags(
        replaceOpenCloseTags(
          tmpl, this.printStartToken, this.genericCloseToken, this.vueInterpolationStartToken, this.vueInterpolationEndToken
        ), this.printLiteralToken, this.genericCloseToken, this.vueInterpolationStartToken, this.vueInterpolationEndToken)
    }

    /**
      * Helper function used to remove chunks of texts btw the given start and end tags
      *
      * @param tmpl
      * @param tagStart
      * @param tagEnd
      * @return
      */
    private def removesSoyTagsElements(tmpl: String, tagStart: String, tagEnd: String): String = {
      if (!tmpl.contains(tagStart)) {
        tmpl
      } else {
        val start = tmpl.lastIndexOf(tagStart)
        val end = tmpl.indexOf(tagEnd, start)
        val before = tmpl.slice(0, start - 1)
        val after = tmpl.slice(end + tagEnd.size, tmpl.size)
        removesSoyTagsElements(before + after, tagStart, tagEnd)
      }
    }

    /**
      * Removes all soy template self params tags
      *
      * @return
      */
    def removesInputParams(tmpl: String) = {
      this.removesSoyTagsElements(tmpl, this.paramStartToken, this.genericCloseToken)
    }

    /**
      * Replaces all call to other soy templates with the similar call on vue js
      */
    def replaceCallsRenderingVueComponents(tmpl: String, cmpDependencyMap: List[(String, Map[String, String])]): String = {
      if (cmpDependencyMap.isEmpty) {
        tmpl
      } else if (cmpDependencyMap.length == 1) {
        val cmpSelector = cmpDependencyMap.head._1.replaceAll(" ", "").replace(".", this.separator)
        val renderedCmpBody = this.renderVueComponent(cmpSelector, cmpDependencyMap.head._2)
        val cmpCallIndex = tmpl.indexOf(this.callOpenToken)
        val cmpCallCloseIndex = tmpl.indexOf(this.callCloseToken, cmpCallIndex)
        tmpl.slice(0, cmpCallIndex - 1) + renderedCmpBody + tmpl.slice(cmpCallCloseIndex + this.callCloseToken.size, tmpl.size)
      } else {
        replaceCallsRenderingVueComponents(
          replaceCallsRenderingVueComponents(tmpl, List(cmpDependencyMap.head)), cmpDependencyMap.tail
        )
      }
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
      * @param singleFile  whether if the component will be in one file or in folder containing 3 files (js, scss, html)
      * @param destination is the destination path where the component will be saved
      */
    def exportVueComponent(tmpl:String,singleFile: Boolean = true, destination: String = "") = {
      val cmpName = (this.getTemplateNamesPace(tmpl) + "-" + this.getTemplateName(tmpl))
        .replace(".", "-")
        .replace(" ", "")
      val cmpTemplate = this.getVueTemplate(tmpl)
      val cmpProps = this.getParams(tmpl)
      val stringComputed = this.getComputedProperties(tmpl).mkString(", \n")
      val stringData = "" // TODO: Calculate data
      val stringProps = cmpProps.mkString(" , ")
      val body = this.vueComponentTemplate
        .replace(this.vueCmpPropsTmplToken, stringProps)
        .replace(this.vueCmpSelectorTmplToken, s"'${cmpName}'")
        .replace(this.vueCmpTemplateTmplToken, s"`${cmpTemplate}`")
        .replace(this.vueCmpComputedTmplToken, s"${stringComputed}")
        .replace(this.vueCmpDataTmplToken, s"${stringData}")
      val filename = {
        if (!destination.isEmpty) s"${destination}/${cmpName}.js" else s"${cmpName}.js"
      }
      val writer = new PrintWriter(new File(filename))
      writer.write(body)
      writer.close()
    }


    /**
      * Get all words of the template
      *
      * @return
      */
    def getRawText(path: String): String = {
      val lines = for (line <- Source.fromFile(path).getLines()) yield {
        line.toString
      }
      lines.mkString
    }




    /**
      * Returns the params defined in the soy template
      *
      * @param optional
      * @return
      */
    def getParams(tmpl: String): List[String] = {
      val typesMap = Map(
        "string" -> "String",
        "html" -> "String",
        "uri"->"String",
        "js"->"String",
        "css"->"String",
        "attributes"->"String","?"->"String",
        "short"->"Number","int"->"Number","long"->"Number","float"->"Number","double"->"Number",
        "bool" -> "Boolean",
        "any" -> "Object","map" -> "Object","null" -> "Object",
        "[" -> "Array","list" -> "Array"
      )

      if (!tmpl.contains(this.paramStartToken)) {
        List()
      } else {
        val paramStart = tmpl.lastIndexOf(this.paramStartToken)
        val paramEnd = tmpl.indexOf(":", paramStart)
        val paramName = tmpl.slice(paramStart + this.paramStartToken.size, paramEnd)
        val paramId = this.sanitize(paramName)
        val isRequired = !paramName.contains("?")

        val newHead = tmpl.slice(0, paramStart - 1)
        val newTail = tmpl.slice(tmpl.indexOf(this.genericCloseToken, paramStart) + this.genericCloseToken.size, tmpl.size)
        val paramSoyType = tmpl.slice(tmpl.indexOf(":", paramStart) + 1, tmpl.indexOf(this.genericCloseToken, paramStart) + this.genericCloseToken.size).trim
        val propType = typesMap.filter(typeMapItem =>  paramSoyType.startsWith(typeMapItem._1)).head._2
        val typeProperty = {
          if (propType != null) s", type: ${propType}" else ""
        }
        val propWrapper = s"${paramId}:{ required: ${isRequired} ${typeProperty} }"
        val newTemplate = newHead + newTail
        List.concat(List(propWrapper), getParams(newTemplate))
      }
    }

    def getTemplateName(tmpl:String): String = {
      getTokenValue(tmpl,"template", "}", true)
    }

    /**
      * Find the value of a token that hold the given start and end identifiers
      *
      * @param startToken
      * @param endToken
      * @return
      */
    def getTokenValue(tmpl:String,startToken: String, endToken: String, sanitizeValue: Boolean = false): String = {
      val tmplStart = tmpl.indexOf(startToken) + startToken.size
      val tmplEnd = tmpl.indexOf(endToken, tmplStart)
      val preResult = tmpl.slice(tmplStart, tmplEnd)
      if (sanitizeValue) sanitize(preResult) else preResult
    }

    /**
      * Returns the namespace of the soy template
      *
      * @return
      */
    def getTemplateNamesPace(tmpl:String): String = {
      getTokenValue(tmpl,"namespace", "}")
    }

    /**
      * Fill the dependencies local parameter
      */
    def findDependencies(template: String) = {
      this.getContentBtwTokens(template, "{call", "}")
    }

    def getComponentBindings2(str: String):List[(String,Map[String,String])]={

      List()
    }
    /**
      * Return a list of Tuple containing each component and it's input props
      *
      * @return
      */
    def getComponentBindings(tmpl: String) = {
      val components = this.getContentBtwTokens(tmpl, this.callOpenToken, this.genericCloseToken)
      val result = for (cmp <- components) yield {
        val subCmpStart = tmpl.indexOf(cmp)
        val subCmpEnd = tmpl.indexOf(this.callCloseToken, subCmpStart)
        val subTemplate = tmpl.slice(subCmpStart, subCmpEnd)
        val paramsMap = this.getContentBtwTokens(subTemplate, this.paramStartInCallToken, this.genericSlashCloseToken, true)
          .filter(p => p.contains(this.colonToken))
          .map(p => {
            val parts = p.split(this.colonToken)
            (parts.head -> parts.tail.head.replaceAll("\\$", ""))
          }).toMap
        (cmp, paramsMap)
      }
      result
    }


    /**
      * Return a list of all props required by the main component
      *
      * @return
      */
    def findParams(template: String): List[String] = {
      this.getContentBtwTokens(template, "@param", ":", true)
    }

    /**
      * Returns a collection of the given tokenNameStart and tokenNameEnd pattern in the soy template
      *
      * @param tmpl
      * @param paramNameStart
      * @param paramNameEnd
      * @param sanitizeValue
      * @return
      */
    private def getContentBtwTokens(tmpl: String, paramNameStart: String, paramNameEnd: String, sanitizeValue: Boolean = false): List[String] = {
      if (!tmpl.contains(paramNameStart)) {
        List()
      } else {
        val paramNameStartPos = tmpl.indexOf(paramNameStart)
        val paramNameEndPos = tmpl.indexOf(paramNameEnd, paramNameStartPos)
        val testValue = tmpl.slice(paramNameStartPos + paramNameStart.size, paramNameEndPos)
        val result = {
          if (sanitizeValue) this.sanitize(testValue) else testValue
        }
        val newTmpl = tmpl.slice(paramNameEndPos + paramNameEnd.size, tmpl.size)
        List(result) ::: getContentBtwTokens(newTmpl, paramNameStart, paramNameEnd, sanitizeValue)
      }
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
    def getVueTemplate(tmpl:String): String = {
      val cmpsAndBindings = this.getComponentBindings(tmpl)
      val template =
        this.lintContent(
          this.translateSpecialTags(
            this.translateSwitchStatements(
              this.translateDeclarations(
                this.translateIfStatements(
                  this.translateLiterals(
                    this.translateForStatements(
                      this.translatePrints(
                        this.replaceCallsRenderingVueComponents(
                          this.removeNamespaceTag(
                            this.removeTemplateTag(
                              this.removesInputParams(
                                tmpl
                              )
                            )
                          )
                          , cmpsAndBindings)
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
      val componentOpenTag = s"<${selector} \n"
      val componentOpenTagClose = ">"
      val componentCloseTag = s"</${selector}>"
      val componentBindings = props.filter(_._1.size > 0).map(p => s"""\tv-bind:${p._1} = "${p._2}" \n""").mkString
      componentOpenTag + componentBindings + componentOpenTagClose + componentCloseTag
    }

    /**
      * Function that serves as function template generator
      *
      * @param name
      * @param body
      * @param params
      * @return
      */
    def getFnDefinition(name: String, body: String, params: List[String] = Nil): String = {
      val p = {
        if (params.isEmpty) "" else params.mkString(", ")
      }
      s"""${name}: function (${p}) {\n return ${body} \n}"""
    }

    /**
      * Removes all Closure `let` declarations from the template
      *
      * @param tmpl String template
      * @return
      */
    def translateDeclarations(tmpl: String): String = {
      if (!tmpl.contains(this.declarationToken)) {
        tmpl
      } else {
        val startPos = tmpl.lastIndexOf(this.declarationToken)
        val arbitraryEndPos = tmpl.indexOf(this.genericSlashCloseToken, startPos)
        val renderingEndPos = tmpl.indexOf(this.declarationEndToken, startPos)
        val endPos = List(arbitraryEndPos, renderingEndPos).filter(_ >= 0).min
        val letBlockEnd = {
          if (endPos == arbitraryEndPos) endPos + this.genericSlashCloseToken.size else endPos + this.declarationEndToken.size
        }
        translateDeclarations(tmpl.slice(0, startPos) + tmpl.slice(letBlockEnd + 1, tmpl.size))
      }
    }

    /**
      * Extract all `let` closure declaration and expose as a computed property
      *
      * @param tmpl String template
      * @return
      */
    def getComputedProperties(tmpl: String): List[String] = {
        if(tmpl.contains(this.declarationToken)){
          val startPos = tmpl.lastIndexOf(this.declarationToken)
          val genericSlashClose = tmpl.indexOf(this.genericSlashCloseToken, startPos)
          val genericClose = tmpl.indexOf(this.genericCloseToken, startPos)
          val colonStart =  tmpl.indexOf(this.colonToken, startPos)
          val declarationEndTag = tmpl.indexOf(this.declarationEndToken, startPos)
          val endDeclarationStart = List(genericSlashClose,genericClose).filter(_>=0).min
          val isArbitrary = endDeclarationStart==genericSlashClose
          val end = {
            if(isArbitrary) genericSlashClose+this.genericSlashCloseToken.size else declarationEndTag+this.declarationEndToken.size
          }
          val fnName = {
            if(isArbitrary)
              tmpl.slice(startPos+this.declarationToken.size,colonStart)
            else{
              val nameStart = tmpl.indexOf(" ",startPos+this.declarationToken.size)
              val spaceAfterName = tmpl.indexOf(" ",nameStart+1)
              tmpl.slice(nameStart,List(spaceAfterName,genericClose).filter(_>=0).min)
            }
          }
          val stringFn = {
            if(isArbitrary)
              this.getFnDefinition(this.sanitize(fnName), tmpl.slice(colonStart+1,genericSlashClose-1))
            else{
              val newTemplate = this.getVueTemplate(tmpl.slice(genericClose+1,declarationEndTag-1))
              this.getFnDefinition(this.sanitize(fnName), s"`${newTemplate}`")
            }
          }
          val tmplWithoutLetBlock = tmpl.slice(0,startPos-1)+tmpl.slice(end,tmpl.size)
          List.concat(List(stringFn), getComputedProperties(tmplWithoutLetBlock))
        }else{
          List()
        }
//      if (tmpl.contains(this.declarationToken)) {
//        val startPos = tmpl.lastIndexOf(this.declarationToken)
//        val arbitraryEndPos = tmpl.indexOf(this.genericSlashCloseToken, startPos)
//        val renderingEndPos = tmpl.indexOf(this.declarationEndToken, startPos)
//        val endPos = List(arbitraryEndPos, renderingEndPos).filter(_ >= 0).min
//        val letBlockEnd = {
//          if (endPos == arbitraryEndPos) endPos + this.genericSlashCloseToken.size else endPos + this.declarationEndToken.size
//        }
//        val letBlock = tmpl.slice(startPos, endPos)
//        val tmplWithoutLetBlock = tmpl.slice(0, startPos) + tmpl.slice(letBlockEnd, tmpl.size)
//        if (endPos == arbitraryEndPos) //arbitrary let declaration
//        {
//          val fnName = letBlock.slice(letBlock.indexOf("$") + 1, letBlock.indexOf(":"))
//          val fnBody = letBlock.slice(letBlock.indexOf(":") + 1, letBlock.size)
//          val stringFn = getFnDefinition(fnName, fnBody)
//          List.concat(List(stringFn), getComputedProperties(tmplWithoutLetBlock))
//        }
//        else {
//          val letBlockDeclaration = letBlock.slice(this.declarationEndToken.size - 1, letBlock.indexOf(this.genericCloseToken) + this.genericCloseToken.size)
//          val firstSpace = letBlockDeclaration.indexOf(" ")
//          val firstGenericClose = letBlockDeclaration.indexOf(this.genericCloseToken)
//          val firstOccurrence = Math.min(firstSpace, firstGenericClose)
//          val nameEnd = {
//            if (firstOccurrence > 0) firstOccurrence else Math.max(firstSpace, firstGenericClose)
//          }
//          val fnName = letBlockDeclaration.slice(letBlockDeclaration.indexOf("$") + 1, nameEnd)
//          val fnBody = letBlock.slice(
//            letBlock.indexOf(this.genericCloseToken) + this.genericCloseToken.size,
//            letBlock.size
//          ).replaceAll("  ", " ")
//          val stringFn = getFnDefinition(fnName, s"`${fnBody}`")
//          List.concat(List(stringFn), getComputedProperties(tmplWithoutLetBlock))
//        }
//      } else {
//        List()
//      }
    }

    /**
      * Translate switch statement into vue if statements using a vue `template` wrapper
      *
      * @param tmpl String template
      * @return
      */
    def translateSwitchStatements(tmpl: String): String = {
      /**
        * Return a List of case wrappers
        *
        * @param switchBlock
        * @param leftSideExpr
        * @param founds
        * @return
        */
      def getCaseBlocks(switchBlock: String, leftSideExpr: String, founds: Int = 0): List[String] = {
        if (!switchBlock.contains(this.caseToken)) {
          List()
        }
        else {
          val caseStart = switchBlock.indexOf(this.caseToken)
          val nextCaseStart = switchBlock.indexOf(this.caseToken, caseStart + 1)
          val defaultStart = switchBlock.indexOf(this.switchDefaultToken, caseStart + 1)
          val closeSwitchTag = switchBlock.indexOf(this.switchCloseToken, caseStart + 1)
          val caseEnd = List(nextCaseStart, defaultStart, closeSwitchTag).filter(_ >= 0).min
          val caseBlock = switchBlock.slice(caseStart, caseEnd)
          val caseContent = caseBlock.slice(caseBlock.indexOf(this.genericCloseToken) + this.genericCloseToken.size, caseBlock.size)
          val caseEvalExpr = this.getContentBtwTokens(caseBlock, this.caseToken, this.genericCloseToken)
            .head.split(",")
            .map(condition => s"${leftSideExpr} == ${condition}")
            .mkString(" || ")

          val newSwitchBlock: String = switchBlock.slice(0, caseStart) + switchBlock.slice(caseEnd, switchBlock.size)
          val ifWrapper = {
            if (founds == 0) this.vIf else this.vElseIf
          }
          val wrapper = s"""${this.vTemplateStart} ${ifWrapper}= "${caseEvalExpr}"> ${caseContent} ${this.vTemplateEnd}"""
          List.concat(List(wrapper), getCaseBlocks(newSwitchBlock, leftSideExpr, founds + 1))
        }

      }

      if (!tmpl.contains(this.switchToken)) {
        tmpl
      } else {
        val switchBlockStart = tmpl.lastIndexOf(this.switchToken)
        val switchBlockEnd = tmpl.indexOf(this.switchCloseToken, switchBlockStart) + this.switchCloseToken.size
        val switchBlock = tmpl.slice(switchBlockStart, switchBlockEnd)
        val switchExpr = this.getContentBtwTokens(switchBlock, this.switchToken, this.genericCloseToken)
          .head.replaceAll("\\$|\\s", "")
        val caseList = getCaseBlocks(switchBlock, switchExpr)
        val defaultCase: String = {
          if (switchBlock.contains(this.switchDefaultToken)) {
            val defaultContent = switchBlock.slice(
              switchBlock.indexOf(this.switchDefaultToken) + this.switchDefaultToken.size,
              switchBlock.indexOf(this.switchCloseToken, switchBlockStart)
            )
            s"""${this.vTemplateStart} ${this.vElse}>${defaultContent}${this.vTemplateEnd}"""
          }
          else {
            ""
          }
        }
        val newHead = tmpl.slice(0, switchBlockStart - 1)
        val newTail = tmpl.slice(switchBlockEnd, tmpl.size)
        val switchWrapper = caseList.mkString("\n") + defaultCase
        translateSwitchStatements(newHead + switchWrapper + newTail)
      }
    }

    /**
      * Translate some special closure tags to the literals in vue js
      *
      * @param tmpl String template
      * @return
      */
    def translateSpecialTags(tmpl: String): String = {
      tmpl.replaceAll(this.spaceToken, " ")
        .replaceAll(this.emptyStringToken, "")
        .replaceAll(this.carriageReturnToken, "\r")
        .replaceAll(this.newLineToken, "\n")
        .replaceAll(this.tabToken, "\t")
        .replaceAll(this.leftBraceToken, "{")
        .replaceAll(this.rightBraceToken, "}")
        .replaceAll(this.logicalAnd, this.vAnd)
        .replaceAll(this.logicalOr, this.vOr)
        .replaceAll(this.logicalNegation, this.vNegation)
    }

  }


  def mkDirs(destFolders: List[String], folder: File): Unit = {
    if (destFolders.isEmpty) return
    val newFolder = new File(folder, destFolders.head)
    if (!folder.list().contains(destFolders.head)) {
      newFolder.mkdir()
      mkDirs(destFolders.tail, newFolder)
    } else {
      mkDirs(destFolders.tail, newFolder)
    }
  }

  def main(args: Array[String]): Unit = {
    try {
      val inputs =Array("src/main/resources/templates/origin","c1.soy")
      if(inputs.length==0){
        throw new Exception(" You need to provide at least the source path of your soy files!!")
      }
      val currentDir = System.getProperty("user.dir")
      val sourcePath = inputs.head
      val cmpFilename = if(inputs.last==inputs.head) "" else inputs.last
      val sourcePathFolder = new File(String.valueOf(s"${currentDir}/${sourcePath}"))
      val generatedPath = "generated"
      val generatedFolder = new File(String.valueOf(generatedPath))
      if (!generatedFolder.exists()) {
        generatedFolder mkdir()
      }

      if (!sourcePathFolder.exists()) {
        throw new Exception("Invalid source path: Source Path not exist!!")
      }
      val filteredSourcePathList = sourcePath.split("/").toList.filter(f => f != ".." && f != ".")
      //check folder structure
      mkDirs(filteredSourcePathList, generatedFolder)
      val translationPath = s"${generatedFolder.getAbsolutePath}/${filteredSourcePathList.mkString("/")}"
      val cmpFilter = if(cmpFilename.length>0) cmpFilename else ".soy"
      val soyFiles = sourcePathFolder.listFiles().filter(_.getName.endsWith(cmpFilter)).map(_.getAbsolutePath)


      for (soy <- soyFiles) {
        try {
          val transInstance = new SoyToVueTranslator(soy)
          val soyContent:String = transInstance.rawText
          transInstance.exportVueComponent(soyContent,true, translationPath)
          println(s"${soy}: Success!!")
        } catch {
          case e: Throwable => println(s"Error translating file ${soy}, Cause: ${e.getMessage}")
        }

      }
    } catch {
      case e: Exception => println(e.getMessage)
        _: Throwable => println("Unknown Error!!")
    }
  }

}
