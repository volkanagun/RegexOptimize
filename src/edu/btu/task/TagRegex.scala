package edu.btu.task

import edu.btu.operands.{RegexGenerator, RegexSingleString, Regexify}
import edu.btu.search.AbstractRegexSearch

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

  def intersect(tagRegex: TagRegex): TagRegex = {

    val intersectedLabels = this.multimap.keySet.intersect(tagRegex.multimap.keySet)
    val intersectedMap = intersectedLabels.map(label => {
      var crrValues = multimap(label)
      var newValues = tagRegex.multimap(label)
      (label -> crrValues.union(newValues))
    }).toMap

    TagRegex(tagName, intersectedMap)

  }

  def difference(tagRegex: TagRegex):TagRegex={
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
