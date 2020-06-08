package edu.btu.task.tagmatch

import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

class TagMain(val tagName: String, val properties: Seq[(String, String)]) extends Serializable {

  def toTagRegex(): TagRegex = {
    val map = properties.map { case (label, value) => label -> Set(value) }.toMap
    new TagRegex(tagName, map)
  }

  def toTagSample(): TagSample = {
    new TagSample(tagName, properties)
  }

  def toTagSample(filename: String, domain: String): TagSample = {
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

  /*def apply(imgStr: String): TagMain = {
    TagParser(imgStr).toTagMain()

  }*/
  def apply(tag: Tag): TagMain = {
    tag.toTagMain()
  }
}

case class Attribute(name: String, value: String) {
  override def toString = s"$name = $value"

  def toPair(): (String, String) = {
    (name, value)
  }
}

case class Tag(name: String, attributes: Seq[Attribute]) {
  def toTagMain(): TagMain = {
    TagMain(name, attributes.map(_.toPair()))
  }
}

class TagParser extends RegexParsers {
  override val whiteSpace = "\\s".r

  def name: Parser[String] = "\\p{L}+".r ^^ {
    _.toString
  }

  /*
    def value: Parser[String] = "[\\p{L}á\\d\\p{Punct}\\/\\_]+".r ^^ {
      _.toString
    }*/

  def value: Parser[String] = "((\\'(.*?)\\')|(\\\"(.*?)\\\"))".r ^^ {
    _.toString
  }

  def attribute: Parser[Attribute] = name ~ "=" ~ value ^^ { case wd ~ "=" ~ vl => Attribute(wd, vl) }

  def tag: Parser[Tag] = "<img " ~> rep[Attribute](attribute) <~ "/>" ^^ { case attrs => Tag("img", attrs) }

}

object TagParser extends TagParser {

  def apply(line: String): Tag = {
    parse(tag, line) match {
      case Success(matched, _) => matched
      case Failure(msg, a) => {
        println(s"ERROR: $line")
        println(s"$a")
        Tag("img", Seq())
      }
      case Error(msg, a) => {
        println(s"ERROR: $line")
        println(s"$a")
        Tag("img", Seq())
      }
    }
  }

  def apply(line: String, lineIndex: Int, filename: String): Tag = {
    parse(tag, line) match {
      case Success(matched, _) => matched
      case Failure(msg, a) => {
        println(s"ERROR: $msg with $lineIndex and in filename: $filename")
        println(s"$a")
        Tag("img", Seq())
      }
      case Error(msg, a) => {
        println(s"ERROR: $msg with $lineIndex and in filename: $filename")
        println(s"$a")
        Tag("img", Seq())
      }
    }
  }

  //read filename
  def read(filename: String): Seq[TagSample] = {
    Source.fromFile(filename, "UTF-8").getLines().map(line => {
      val Array(file, domain, image) = line.split("\\s+")
      TagSample(image, filename, domain)
    }).toSeq
  }

  //read CSV
  def readCSV(filename: String): Seq[TagSample] = {

    println(s"Reading filename: ${filename}")
    val lines = Source.fromFile(filename, "UTF-8").getLines().toArray.filter(!_.trim.isEmpty)
    val names = lines.head.split("(\\s+|\\t+)").filter(!_.contains("theImg"))
    val regex = "(\"\\<(.*?)\\>\")"

    lines.tail.zipWithIndex.map { case (line, index) => {

      val linem = line.replaceAll("(\\/)?>", ">")
      var imageLines = regex.r.findAllIn(linem).toArray

      var images = imageLines.map(imgLine => {
        val nline = imgLine.replaceAll("\"\"", "\"").trim;
        nline.substring(1, nline.length - 1)
      })

      val image = images.head
      val parent1 = images(1)
      val parent2 = images(2)

      val values = line.replaceAll(regex, "\t").split("\\s+")
      var mapping = names.zip(values).map { case (key, value) => key -> value }.toMap

      mapping = mapping.updated("Parent1", parent1)
      mapping = mapping.updated("Parent2", parent2)
      mapping = mapping.updated("theImg", image)

      val filename = mapping("Number")
      val domain = mapping("WebSite")
      val negative = mapping("main_image").toInt == 0
      val tag = apply(image, index, filename)

      TagSample(tag, filename, domain, negative)

    }
    }.toSeq

  }


  def main(args: Array[String]): Unit = {
    parse(tag, "<img alt=\"La luz laluz cmp\" src=\"/images/stories/2019/Diciembre/La-luz-laluz-cmp-01.jpg\" style=\"width:100%;\" title='Ediciones La Luz presenta la campaña de comunicación del 2020  \"A la Luz se lee mejor\".' />") match {
      case Success(matched, a) => println(matched)
      case Failure(msg, a) => println(s"FAILURE: $msg  ==> " + a.offset)
      case Error(msg, a) => println(s"ERROR: $msg" + a.offset)
    }

    readCSV("resources/img-csvs/ahora.txt")
  }
}