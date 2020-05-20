package edu.btu.task.tagmatch

import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

class TagMain(val tagName: String, val properties: Seq[(String, String)]) extends Serializable {

  def toTagRegex(): TagRegex = {
    val map = properties.map { case (label, value) => label -> Set(value) }.toMap
    new TagRegex(tagName, map)
  }


  def toTagSample(filename:String, domain:String):TagSample={
    new TagSample(tagName, properties)
      .setFilename(filename)
      .setDomain(domain)
  }

  override def toString: String = {
    val content = properties.map { case (attr, value) => attr + "=\"" + value + "\"" }.mkString(" ")
    "<" + tagName + " " + content + ">"
  }


  def toOpenRegex: String = {
    val content = properties.map { case (attr, value) => attr + "\\=\"" + value + "\"" }.mkString(" ")
    "<" + tagName + " " + content + ">"
  }

  def toCloseRegex: String = {
    val content = properties.map { case (attr, value) => attr + "\\=\"" + value + "\"" }.mkString(" ")
    "</" + tagName + ">"
  }

  def toOpenCloseRegex: String = {
    val content = properties.map { case (attr, value) => attr + "\\=\"" + value + "\"" }.mkString(" ")
    "<" + tagName + " " + content + "/>"
  }


}

object TagMain {

  def apply(tagName: String, properties: Seq[(String, String)]): TagMain = {
    new TagMain(tagName, properties)
  }

  def apply(imgStr: String): TagMain = {
    TagParser(imgStr).toTagMain()

  }
}

case class Attribute(name: String, value: String) {
  override def toString = s"$name = $value"
  def toPair():(String, String)={
    (name, value)
  }
}

case class Tag(name: String, attributes: Seq[Attribute]){
  def toTagMain():TagMain={
    TagMain(name, attributes.map(_.toPair()))
  }
}

class TagParser extends RegexParsers {
  override val whiteSpace = "\\s".r

  def name: Parser[String] = "[a-z]+".r ^^ {
    _.toString
  }

  def value: Parser[String] = "[a-z\\d\"]+".r ^^ {
    _.toString
  }

  def attribute: Parser[Attribute] = name ~ "=" ~ value ^^ { case wd ~ "=" ~ vl => Attribute(wd, vl) }

  def tag: Parser[Tag] = "<img " ~> rep[Attribute](attribute) <~ ">" ^^ { case attrs => Tag("img", attrs) }
}

object TagParser extends TagParser {

  def apply(line: String): Tag = {
    parse(tag, line) match {
      case Success(matched, _) => matched
      case Failure(msg, _) => {
        println(s"ERROR: $line")
        Tag("img", Seq())
      }
      case Error(msg, _) => {
        println(s"ERROR: $line")
        Tag("img", Seq())
      }
    }
  }

  //read directory
  def read(filename:String):Seq[TagSample]={
    Source.fromFile(filename, "UTF-8").getLines().map(line=> {
      val Array(file, domain, image) = line.split("\\s+").toArray
      TagSample(image,filename, domain)
    }).toSeq
  }



  def main(args: Array[String]) = {
    parse(tag, "<img class=\"deneme\" alt=\"100\" ban=\"23\">") match {
      case Success(matched, _) => println(matched)
      case Failure(msg, _) => println(s"FAILURE: $msg")
      case Error(msg, _) => println(s"ERROR: $msg")
    }
  }
}