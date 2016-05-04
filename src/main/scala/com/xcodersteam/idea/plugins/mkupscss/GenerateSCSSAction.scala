package com.xcodersteam.idea.plugins.mkupscss


import java.awt.datatransfer.StringSelection
import java.util.regex.Pattern

import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent, CommonDataKeys}
import com.intellij.openapi.ide.CopyPasteManager
import com.intellij.openapi.ui.popup.JBPopupFactory
import com.intellij.openapi.util.text.StringUtil
import com.intellij.openapi.wm.WindowManager


/**
  * XCodersTeam 2016 MarkupToSCSS
  * Created by semoro on 3/25/16.
  */
class GenerateSCSSAction extends AnAction {

  val pattern = Pattern.compile("<[^>]*>")

  override def update(e: AnActionEvent): Unit = {
    val project = e.getData(CommonDataKeys.PROJECT)
    val editor = e.getData(CommonDataKeys.EDITOR)
    val lang = e.getData(CommonDataKeys.PSI_FILE).getFileType.getName
    e.getPresentation.setVisible(CodeLangHelper.langs.get(lang).isDefined && project != null && editor != null)
    e.getPresentation.setEnabled(editor.getSelectionModel.hasSelection() && pattern.matcher(editor.getSelectionModel.getSelectedText).find())
  }

  override def actionPerformed(anActionEvent: AnActionEvent): Unit = {
    val editor = anActionEvent.getData(CommonDataKeys.EDITOR)
    val lang = anActionEvent.getData(CommonDataKeys.PSI_FILE).getFileType.getName
    val text = editor.getSelectionModel.getSelectedText()

    val statusBar = WindowManager.getInstance().getStatusBar(editor.getProject)
    val codeLang = CodeLangHelper.langs.get(lang).orNull
    val prefix = "[" + codeLang.name + "->SCSS]"
    try {
      if (codeLang == null)
        throw new CodeConvertingException(message = "Failed to recognize file type")
      val generated = CodeConverter(codeLang.prepareText(text), codeLang)
      CopyPasteManager.getInstance().setContents(new StringSelection(StringUtil.convertLineSeparators(generated, "\n")))
      JBPopupFactory.getInstance()
        .createHtmlTextBalloonBuilder(prefix + " Successful generated scss and copied to clipboard", com.intellij.openapi.ui.MessageType.INFO, null)
        .setFadeoutTime(5000)
        .createBalloon().showInCenterOf(statusBar.getComponent)
    } catch {
      case e: CodeConvertingException =>
        JBPopupFactory.getInstance()
          .createHtmlTextBalloonBuilder(e.errorMessage, com.intellij.openapi.ui.MessageType.ERROR, null)
          .setFadeoutTime(5000)
          .createBalloon().showInCenterOf(statusBar.getComponent)
    }

  }
}
