package com.xcodersteam.idea.plugins.mkupscss



import java.awt.datatransfer.StringSelection

import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent, CommonDataKeys}
import com.intellij.openapi.ide.CopyPasteManager
import com.intellij.openapi.ui.popup.JBPopupFactory
import com.intellij.openapi.util.text.StringUtil
import com.intellij.openapi.wm.WindowManager

import scala.xml._


/**
  * XCodersTeam 2016 JSXtoSCSS
  * Created by semoro on 3/25/16.
  */
class GenerateSCSSAction extends AnAction{

  override def update(e: AnActionEvent): Unit = {
    val project = e.getData(CommonDataKeys.PROJECT)
    val editor = e.getData(CommonDataKeys.EDITOR)
    e.getPresentation.setVisible(project!=null && editor!=null)

    val lang = e.getData(CommonDataKeys.PSI_FILE).getFileType.getName
    e.getPresentation.setEnabled(AttributesHelper.attributesNameDefs.get(lang).isDefined && editor.getSelectionModel.hasSelection())
  }

  def parseNodeList(tabs:Int,buffer: Seq[Node]): String = {
    val sb = new StringBuilder("")
    buffer.foreach(node => sb.append(parseNode(tabs,node)))
    sb.toString()
  }

  def mkTabs(tabs:Int, sb: StringBuilder): Unit = {
    for(i <- 1 to tabs)
      sb.append("    ")
  }

  def parseNode(tabs: Int,node: Node): String = {

    val sb = new StringBuilder("")
    val isTag = node.label.charAt(0).isLower
    if(isTag) {
      mkTabs(tabs,sb)
      if(node.label == null)
        return ""
      if(node.label!="div")
        sb.append(node.label)
      val cn = node.attribute(attributesNameDef.classname)
      if (cn.isDefined)
        cn.get.toString.split(" ").foreach(n => sb.append(".").append(n))
      val id = node.attribute(attributesNameDef.id)
      if (id.isDefined)
        sb.append("#").append(id.get.toString)
      sb.append("{")
    }
    var str = ""
    if(node.child.nonEmpty) {
      str = parseNodeList(tabs + 1, node.child)
    }
    if(str.length>0) {
      sb.append("\n")
      sb.append(str)
    }
    if(isTag) {
      if(str.length > 0)
        mkTabs(tabs,sb)
      sb.append("}\n")
    }
    sb.toString()
  }

  var attributesNameDef: AttributesNameDef = null
  override def actionPerformed(anActionEvent: AnActionEvent): Unit = {
    val editor = anActionEvent.getData(CommonDataKeys.EDITOR)
    val lang = anActionEvent.getData(CommonDataKeys.PSI_FILE).getFileType.getName
    val text = editor.getSelectionModel.getSelectedText()

    val statusBar = WindowManager.getInstance().getStatusBar(editor.getProject)
    var errorMsg = ""
    attributesNameDef = AttributesHelper.attributesNameDefs.get(lang).orNull
    if(attributesNameDef!=null) {
      val prefix = "["+attributesNameDef.name+"->SCSS]"
      try {
        val xml = XML.loadString(attributesNameDef.prepareText(text))
        val generated = parseNodeList(0, xml)
        CopyPasteManager.getInstance().setContents(new StringSelection(StringUtil.convertLineSeparators(generated, "\n")))
        JBPopupFactory.getInstance()
          .createHtmlTextBalloonBuilder(prefix+" Successful generated scss and copied to clipboard", com.intellij.openapi.ui.MessageType.INFO, null)
          .setFadeoutTime(5000)
          .createBalloon().showInCenterOf(statusBar.getComponent)
      } catch {
        case ex: SAXParseException => errorMsg = prefix+" Parsing exception: " + ex.getMessage
        case exz: SAXException => errorMsg = prefix+" SAXException lol?! " + exz.getMessage
        case e: Exception => errorMsg = prefix+" Parsing failed."
      }
    }else
      errorMsg = "Can't recognize file type"
    if(errorMsg!=""){
      JBPopupFactory.getInstance()
        .createHtmlTextBalloonBuilder(errorMsg,com.intellij.openapi.ui.MessageType.ERROR, null)
        .setFadeoutTime(5000)
        .createBalloon().showInCenterOf(statusBar.getComponent)
    }


  }
}
