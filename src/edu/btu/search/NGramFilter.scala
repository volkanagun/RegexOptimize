package edu.btu.search

import edu.btu.operands.RegexNodeIndex

class NGramFilter(val ratio:Double) {
  //the regex must fill in the blanks
  //multiple regular expressions
  //generate most common expressions as n-gram dictionary
  //filter expressions by those common n-grams/ extract regex
  var freqDictionary = Map[String, Int]()
  var stepSize = 3;
  var sliceSize = 5;
  var acceptRatio = ratio


  /**
   * Generate n-grams
   * @param item
   * @return
   */
  def ngrams(item:String):Set[String] = {
    val slices = item.sliding(sliceSize, stepSize).toSet
    slices
  }

  def accept(item: String): Boolean = {
    val slices = ngrams(item)
    val ratio = slices.map(slice=> freqDictionary.getOrElse(slice, 0)).sum.toDouble / slices.size
    ratio > acceptRatio
  }

  def filter(items:Set[String]): Set[String] = {

    items.foreach(item=> {
    val slices = ngrams(item)
      slices.foreach(slice=> {freqDictionary = freqDictionary.updated(slice, freqDictionary.getOrElse(slice, 0)+1)})
    })

    items.filter(accept(_))

  }
}
