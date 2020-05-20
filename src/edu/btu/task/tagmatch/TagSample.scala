package edu.btu.task.tagmatch

class TagSample(override val tagName:String, override val properties: Seq[(String, String)]) extends TagMain(tagName, properties) {
  var filename:String = null
  var domain:String = null
  var positiveRegex:TagRegex = toTagRegex()
  var negativeRegex:TagRegex = toTagRegex()




  def setFilename(filename:String):this.type ={
    this.filename = filename
    this
  }

  def setDomain(domain:String):this.type ={
    this.domain = domain
    this
  }

  def intersect(tagRegex: TagRegex): this.type ={
    this.positiveRegex.intersect(tagRegex)
    this
  }

  def difference(tagRegex: TagRegex): this.type ={
    this.negativeRegex.difference(tagRegex)
    this
  }

  //create multiple regexes

  //create single regexes

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

object TagSample{
  def apply(imgStr:String, filename:String, domain:String):TagSample={
    TagMain(imgStr)
      .toTagSample(filename, domain)
  }
}
