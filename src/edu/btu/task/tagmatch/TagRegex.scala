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
class TagRegex(val tagName: String, var multimap: Map[String, Set[String]]) extends Serializable {

  var mapPositiveRegex = Map[String, Set[String]]()
  var mapNegativeRegex = Map[String, Set[String]]()

  def filter():TagRegex={
    multimap = multimap.filter{case(key, values)=> !values.isEmpty}
    this
  }

  def unisect(tagRegexes:Seq[TagRegex]):TagRegex = {
    val unisected = tagRegexes.foldRight[TagRegex](this){ case(right, main)=> main.unisect(right)}
    unisected
  }

  def intersect(tagRegexes:Seq[TagRegex]):TagRegex = {
    val intersected = tagRegexes.foldRight[TagRegex](this){ case(right, main)=> main.intersect(right)}
    intersected
  }

  def difference(tagRegexes:Seq[TagRegex]):TagRegex ={
    val difference = tagRegexes.foldRight[TagRegex](this){ case(right, main)=> main.difference(right)}
    difference
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

  def unisect(tagRegex: TagRegex): TagRegex = {

    val unionLabels = this.multimap.keySet.union(tagRegex.multimap.keySet)
    val intersectedMap = unionLabels.map(label => {
      if(multimap.contains(label) && tagRegex.multimap.contains(label)) {
        var crrValues = multimap(label)
        var newValues = tagRegex.multimap(label)
        (label -> crrValues.union(newValues))
      }
      else if(multimap.contains(label)){
        (label -> multimap(label))
      }
      else{
        (label -> tagRegex.multimap(label))
      }
    }).toMap

    TagRegex(tagName, intersectedMap)

  }

  def difference(tagRegex: TagRegex):TagRegex = {

    val intersectedLabels = this.multimap.keySet.union(tagRegex.multimap.keySet)
    val intersectedMap = intersectedLabels.map(label => {
      if(multimap.contains(label) && tagRegex.multimap.contains(label)) {
        var crrValues = multimap(label)
        var newValues = tagRegex.multimap(label)
        (label -> newValues.diff(crrValues))
      }
      else if(multimap.contains(label)){
        (label -> Set[String]())
      }
      else{
        (label -> tagRegex.multimap(label))
      }
    }).toMap

    TagRegex(tagName, intersectedMap)

  }


}

object TagRegex {
  def apply(tagName:String, multimap:Map[String, Set[String]]):TagRegex={
    new TagRegex(tagName, multimap)
  }
}
