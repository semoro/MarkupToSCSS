package com.xcodersteam.idea.plugins.mkupscss

import java.util.regex.Pattern

/**
  * XCodersTeam 2016 MarkupToSCSS
  * Created by semoro on 3/25/16.
  */

class CodeLangNamings(val classname: String, val id: String)

trait CodeLang {
  val name: String
  val namings: CodeLangNamings

  def prepareText(code: String): String
}

class HTMLLang extends CodeLang {

  override val name: String = "HTML"
  override val namings: CodeLangNamings = new CodeLangNamings("class", "id")
  val garbageAttributes = List("required", "multiple", "spellcheck", "hidden")

  override def prepareText(code: String): String = {
    var text = code
    garbageAttributes.foreach((attr) => text = text.replaceAll(attr, ""))
    text
  }
}

class JSXLang extends HTMLLang {
  override val name: String = "JSX"
  override val namings: CodeLangNamings = new CodeLangNamings("className", "id")

  override def prepareText(t: String): String = {
    var text = t
    val pattern = Pattern.compile("\\{[^\\{\\}]*?\\}")
    val matcher = pattern.matcher(text)
    do {
      text = pattern.matcher(text).replaceAll("\"\"")
      matcher.reset(text)
    } while (matcher.find())

    val a = text.indexOf('(')
    val b = text.lastIndexOf(')')
    text = text.substring(if (a >= 0) a + 1 else 0, if (b >= 0) b - 1 else text.length)
    text = super.prepareText(text)
    text
  }
}

class PHPLang extends HTMLLang {
  override val name: String = "PHP"

  override def prepareText(code: String): String =
    super.prepareText(code
      .replaceAll("<\\?.*?\\?>", "")
      .replaceAll("<%.*?%>", "")
      .replaceAll("<script language=(\'|\")php(\'|\")>.*?</script>", "")
    )
}

object CodeLangHelper {
  val jsx = new JSXLang
  val langs = Map(
    "JSX Harmony" -> jsx,
    "HTML" -> new HTMLLang,
    "PHP" -> new PHPLang,
    "JavaScript" -> jsx
  )
}
