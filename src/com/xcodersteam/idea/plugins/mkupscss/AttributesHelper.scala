package com.xcodersteam.idea.plugins.mkupscss

import java.util.regex.Pattern

/**
  * XCodersTeam 2016 JSXtoSCSS
  * Created by semoro on 3/25/16.
  */

class AttributesNameDef(val name: String, val classname: String, val id:String, val garbageAttributes: List[String]) {
  def prepareText(t: String): String = {
    var text = t
    garbageAttributes.foreach((attr) => text=text.replaceAll(attr,""))
    text
  }
}

class JSXAttributesNameDef extends AttributesNameDef("JSX","className","id",List("required","multiple","spellcheck","hidden")){
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
    text=text.substring(if (a>=0) a+1 else 0, if(b>=0) b-1 else text.length)
    text = super.prepareText(text)
    text
  }
}

object AttributesHelper {
  val attributesNameDefs = Map(
      "JSX Harmony" -> new JSXAttributesNameDef(),
      "HTML" -> new AttributesNameDef("HTML","class","id",List("required","multiple","spellcheck","hidden"))
    )
}
