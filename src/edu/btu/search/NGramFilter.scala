package edu.btu.search

import edu.btu.operands.RegexNodeIndex
import edu.btu.task.tagmatch.TagExperimentCodes

class NGramFilter(var patternFilterRatio:Double) extends Serializable {
  //the regex must fill in the blanks
  //multiple regular expressions
  //generate most common expressions as n-gram dictionary
  //filter expressions by those common n-grams/ extract regex
  var freqDictionary = Map[String, Int]()
  var stepSize = TagExperimentCodes.ngramStepLength;
  var sliceSize = TagExperimentCodes.ngramLength;

  var topCount = 50;

  def setTopCount(count:Int):this.type={
    this.topCount = count;
    this
  }

  def setStepSize(stepSize:Int):this.type={
    this.stepSize = stepSize;
    this
  }

  def setSliceSize(sliceSize:Int):this.type={
    this.sliceSize = sliceSize;
    this
  }

  def setPatternFilterRatio(ratio:Double):this.type={
    this.patternFilterRatio = ratio;
    this
  }

  /**
   * Generate n-grams
   * @param item
   * @return
   */
  def ngrams(item:String):Set[String] = {
    val nitem = clean(item)
    val slices = nitem.sliding(sliceSize, stepSize).toSet
    slices
  }

  def clean(item:String):String={
    item.replaceAll("\"", "")
      .replaceAll("\\d","1").replaceAll("\\p{Punct}","!")
  }

  //accept an attribute value as representative
  def accept(item: String): Boolean = {
    val slices = ngrams(item)
    val ratio = slices.map(slice=> freqDictionary.getOrElse(slice, 0)).sum.toDouble / slices.size
    ratio > patternFilterRatio
  }

  //create ngrams from positive or negative samples
  //keep most (idf) frequent n-gram patterns
  def slice(items:Set[String]):Set[String] = {
    items.flatMap(item=> {
      val nslices = ngrams(item)
      nslices.map(slice=> (slice, freqDictionary.getOrElse(slice, 0)))
    }).toSeq.sortBy(_._2)
      .reverse.map(_._1).take(topCount)
      .toSet
  }

  def count(items:Set[String]):this.type ={

    items.foreach(item=> {
      val slices = ngrams(item)
      slices.foreach(slice=> {freqDictionary = freqDictionary.updated(slice, freqDictionary.getOrElse(slice, 0)+1)})
    })

    //topCount = Math.min((freqDictionary.size * acceptRatio).toInt+1, topCount)
    this
  }

  def filter(items:Set[String]): Set[String] = {
    items.filter(accept(_))

  }

  def filterSlice(items:Set[String]): Set[String] = {
    val filtered = items.filter(accept(_))
    slice(filtered)
  }


}

object NGramFilter extends NGramFilter(TagExperimentCodes.patternFilterRatio){
  def apply() = this
}
