package com.xcodersteam.idea.plugins.mkupscss

import scala.xml._

/**
  * XCodersTeam 2016 MarkupToSCSS
  * Created by semoro on 3/29/16.
  */


class CodeNode(node: Node, lang: CodeLang) {
  val tag = node.label
  val classNames = if (classnameAttribute.isDefined) Option(classnameAttribute.get.toString.split(" ").toSeq) else Option.empty
  val id = node.attributes.get(lang.namings.id)
  val children = if (shouldDigDeeper) node.child.map((node) => new CodeNode(node, lang)) else null
  private val classnameAttribute = node.attributes.get(lang.namings.classname)

  def isJSXNode: Boolean = if (lang == CodeLangHelper.langs.get("JSX Harmony").get) !isTag else false

  def shouldRender: Boolean = !isText && !isCDATA && isTag && !(isDiv && classNames.isEmpty && id.isEmpty)

  def isText: Boolean = tag == null || tag.length == 0

  def isTag: Boolean = tag.charAt(0).isLower

  def isDiv: Boolean = tag.toLowerCase == "div"

  def isCDATA: Boolean = tag equalsIgnoreCase "#PCDATA"

  def shouldDigDeeper: Boolean = !isText && node.child.nonEmpty
}

class CodeConvertingException(base: Exception = null, message: String = null) extends Exception {
  val errorMessage: String = base match {
    case null => message
    case e: SAXParseException => "Parsing exception at " + e.getLineNumber + ":" + e.getColumnNumber + " " + e.getMessage
    case e: SAXException => "SAXException lol?! " + e.getMessage
    case e: Exception => "Parsing failed."
  }
}

object CodeConverter {

  def mkTabs(tabs: Int, sb: StringBuilder): Unit = {
    for (i <- 1 to tabs)
      sb.append("    ")
  }


  def renderNode(out: StringBuilder, node: CodeNode, tabs: Int): Unit = {
    if (!node.shouldRender) {
      if (node.shouldDigDeeper)
        node.children.foreach(renderNode(out, _, tabs))
      return
    }
    mkTabs(tabs, out)
    if (!node.isDiv)
      out ++= node.tag
    if (node.id.isDefined)
      out ++= "#" ++= node.id.get.toString
    if (node.classNames.isDefined)
      node.classNames.get.foreach((className) => out ++= "." ++= className)
    val subBlock = new StringBuilder()
    if (node.shouldDigDeeper)
      node.children.foreach(renderNode(subBlock, _, tabs + 1))
    if (subBlock.nonEmpty) {
      out ++= " {\n" ++= subBlock
      mkTabs(tabs, out)
      out ++= "}\n"
    } else {
      out ++= " {}\n"
    }
  }

  def apply(code: String, lang: CodeLang): String = {
    try {
      val xml = XML.loadString("<div>" + code + "</div>") // Wrap code into div block to fix problem, when multiple root element will throw exception
      val root = new CodeNode(xml, lang)
      val sb = new StringBuilder()
      renderNode(out = sb, node = root, tabs = 0)
      sb.toString()
    } catch {
      case e: Exception => throw new CodeConvertingException(e)
    }
  }
}
