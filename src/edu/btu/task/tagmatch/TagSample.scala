package edu.btu.task.tagmatch

import edu.btu.search.NGramFilter

class TagSample(override val tagName: String, override val properties: Seq[(String, String)]) extends TagMain(tagName, properties) {

  var filename: String = null
  var domain: String = null
  var positiveRegex: TagRegex = toTagRegex()
  var negativeRegex: TagRegex = toTagRegex()
  var isNegative: Boolean = false
  val ngramFilter = NGramFilter()

  var psMap = properties.map { case (k, v) => k -> ngramFilter.clean(v) }.toMap

  def regexBinCount(regex:String, propertyValue:String):Double ={
    val sz = regex.r.findAllIn(propertyValue).size
    if(sz >= 2) 2.0
    else if(sz >= 1) 1.0
    else 0.0
  }

 def regexBinBinary(regex:String, propertyValue:String):Double ={
    val sz = regex.r.findAllIn(propertyValue).size
    if(sz > 0) 1.0
    else 0.0
  }

  //positive training average to decide
  def matchWithPositive(regexMap: Map[String, Set[String]], defaultValue: Boolean = true): Double = {
    var count = 0.0
    var size = 0

    regexMap.foreach { case (item, regexSet) => {
      if(psMap.contains(item)) {
        val propertyValue = psMap(item)
        count += regexSet.map(regex => regexBinCount(regex, propertyValue)).sum
      }
      size += regexSet.size
    }}

    val result = (count / size)
    result
  }

  //should not match any of negative
  def matchWithNegative(regexPositive: Map[String, Set[String]], regexNegative: Map[String, Set[String]], defaultValue: Boolean = true): (Double, Double) = {

    var count = 1.0
    var nocount = 1.0
    var size = 1.0
    var nosize = 1.0

    regexPositive.foreach { case (item, regexSet) => {
      if(psMap.contains(item)) {
        val propertyValue = psMap(item)
        count += regexSet.map(regex => regexBinBinary(regex, propertyValue)).sum
        size += regexSet.size
      }

    }}

    regexNegative.foreach { case (item, regexSet) => {
      if(psMap.contains(item)) {
        val propertyValue = psMap(item)
        nocount += regexSet.map(regex => regexBinBinary(regex, propertyValue)).sum
        nosize += regexSet.size
      }

    }}

    val yesmatch = count.toDouble/size + 1E-13
    val nomatch = nocount.toDouble/nosize + 1E-13

    (yesmatch, nomatch)
  }

  def setNegative(): this.type = {
    this.isNegative = true
    this
  }

  def setNegative(value: Boolean): this.type = {
    this.isNegative = value
    this
  }

  def setFilename(filename: String): this.type = {
    this.filename = filename
    this
  }

  def setDomain(domain: String): this.type = {
    this.domain = domain
    this
  }

  def filter(): this.type = {
    this.positiveRegex.filter()
    this.negativeRegex.filter()
    this

  }

  def intersect(tagRegex: TagRegex): this.type = {
    this.positiveRegex = this.positiveRegex.intersect(tagRegex)
    this
  }

  def unisect(tagRegex: TagRegex): this.type = {
    this.positiveRegex = this.positiveRegex.unisect(tagRegex)
    this
  }

  def intersect(tagRegexes: Seq[TagRegex]): this.type = {
    this.positiveRegex = this.positiveRegex.intersect(tagRegexes)
    this
  }

  def unisect(tagRegexes: Seq[TagRegex]): this.type = {
    this.positiveRegex = this.positiveRegex.unisect(tagRegexes)
    this
  }

  def intersectBySamples(tagSamples: Seq[TagSample]): this.type = {
    this.positiveRegex = this.positiveRegex.intersect(tagSamples.map(_.positiveRegex))
    this
  }

  def unisectBySamples(tagSamples: Seq[TagSample]): this.type = {
    this.positiveRegex = this.positiveRegex.unisect(tagSamples.map(_.positiveRegex))
    this
  }

  def difference(tagRegex: TagRegex): this.type = {
    this.negativeRegex = this.negativeRegex.difference(tagRegex)
    this
  }

  def difference(tagRegexes: Seq[TagRegex]): this.type = {
    this.negativeRegex = this.negativeRegex.difference(tagRegexes)
    this
  }

  def differenceBySamples(tagSamples: Seq[TagSample]): this.type = {
    this.negativeRegex = this.negativeRegex.difference(tagSamples.map(_.positiveRegex))
    this
  }

  def createRegex(): String = {
    "<" + tagName + "(.*?)>"
  }


  def canEqual(other: Any): Boolean = other.isInstanceOf[TagSample]

  override def equals(other: Any): Boolean = other match {
    case that: TagSample =>
      (that canEqual this) &&
        filename == that.filename &&
        domain == that.domain &&
        tagName == that.tagName &&
        properties == that.properties
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(filename, domain, tagName, properties)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

object TagSample {

  def apply(tagName: String): TagSample = {
    TagMain(tagName, Seq()).toTagSample()
  }

  def apply(imgStr: String, filename: String, domain: String): TagSample = {
    val tag = TagParser.apply(imgStr)
    TagMain(tag)
      .toTagSample(filename, domain)
  }

  def apply(imgStr: String, filename: String, domain: String, negative: Boolean): TagSample = {
    val tag = TagParser.apply(imgStr)
    TagMain(tag)
      .toTagSample(filename, domain).setNegative(negative)
  }

  def apply(tag: Tag, filename: String, domain: String, negative: Boolean): TagSample = {
    TagMain(tag)
      .toTagSample(filename, domain).setNegative(negative)
  }

}
