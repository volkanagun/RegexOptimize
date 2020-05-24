package edu.btu.task.tagmatch

// Our interface
trait Monoid[A] {
  def zero: A
  def plus(a: A, b: A): A
}

// Implementation for integers
object IntegerMonoid extends Monoid[Int] {
  override def zero: Int = 0
  override def plus(a: Int, b: Int): Int = a + b
}

// Implementation for strings
object StringMonoid extends Monoid[String] {
  override def zero: String = ""
  override def plus(a: String, b: String): String = a.concat(b)
}



object Convertor{
  def sum[A](values: Seq[A])(implicit ev: Monoid[A]): A = values.foldLeft(ev.zero)(ev.plus)
}




//use implicit conversion
class TagRegex(val tagName: String, val multimap: Map[String, Set[String]]) extends Serializable {

  var mapPositiveRegex = Map[String, Set[String]]()
  var mapNegativeRegex = Map[String, Set[String]]()

  def intersect(tagRegexes:Seq[TagRegex]):this.type = {
    tagRegexes.foreach{ right=> intersect(right)}
    this
  }

  def difference(tagRegexes:Seq[TagRegex]):this.type ={
    tagRegexes.foreach{right => difference(right)}
    this
  }

  def intersect(tagRegex: TagRegex): TagRegex = {

    val intersectedLabels = this.multimap.keySet.intersect(tagRegex.multimap.keySet)
    val intersectedMap = intersectedLabels.map(label => {
      var crrValues = multimap(label)
      var newValues = tagRegex.multimap(label)
      (label -> crrValues.union(newValues))
    }).toMap

    TagRegex(tagName, intersectedMap)

  }

  def difference(tagRegex: TagRegex):TagRegex = {
    val intersectedLabels = this.multimap.keySet.intersect(tagRegex.multimap.keySet)
    val intersectedMap = intersectedLabels.map(label => {
      var crrValues = multimap(label)
      var newValues = tagRegex.multimap(label)
      (label -> newValues.diff(crrValues))
    }).toMap

    TagRegex(tagName, intersectedMap)

  }


}

object TagRegex {
  def apply(tagName:String, multimap:Map[String, Set[String]]):TagRegex={
    new TagRegex(tagName, multimap)
  }
}
