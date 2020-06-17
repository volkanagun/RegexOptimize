package edu.btu.operands

import edu.btu.search.{AbstractRegexSearch, MultiPositiveApprox, MultiPositiveExact, NGramFilter, SinglePositiveApprox, SinglePositiveExact}
import edu.btu.task.tagmatch.{TagExperimentCodes, TimeBox}

import scala.util.Random

class RegexSingleString(val regexSearch: AbstractRegexSearch, val ratio: Double = 0.0, val count: Int = 20) extends RegexGenerator(ratio, count) {

  def filter(set: Set[String]): Set[String] = {
    set.filter(regex => {
      val matchAll = positives.forall(positive => positive.matches(regex))
      matchAll
    })
  }

  def generateTimely(): Set[String] = {
    System.out.println("Generating single-regex...")
    val set = TimeBox.measureTime[Set[String]]("Regex-Single", generate())
    filterTimely(set)
  }

  def filterTimely(set: Set[String]): Set[String] = {
    TimeBox.measureTime[Set[String]]("Regex-Single-Filter", filter(set))
  }

  def generate(): Set[String] = {

    val paths = regexSearch.addPositive(positives)
      .searchDirectional()
      .sortBy(_.cost)
      .toArray
      .take(TagExperimentCodes.maxNodes)

    val regexNodes = paths.map(crrPath => {
      crrPath.toOrRegex().constructRegexNode()
    }).distinct

    val indices = for (x <- 0 until regexNodes.length; y <- 0 until regexNodes.length) yield (x, y)
    val elems = indices.filter { case (x, y) => x != y }
      .map { case (x, y) => (regexNodes(x).combineOrNode(regexNodes(y))) }

    val regexes = elems.map(nodeIndex => nodeIndex.toRegex()).toSet

    if(regexNodes.isEmpty){
      Set()
    }
    else if (regexes.isEmpty) {
      val singleRegex = regexNodes.head.toRegex()
      Set(singleRegex)
    }
    else {
      Set(longest(regexes))
    }
  }

  /*Randomization can be applied later
  def generatePositives(random:Random):Set[String] = {
    val paths = regexSearch.addPositive(positives)
      .searchDirectional()
      .sortBy(_.cost)
      .toArray

    val regexNodes = paths.map(crrPath => {
      crrPath.toOrRegex().constructRegexNode()
    }).distinct

    val indices = for(x<-0 until regexNodes.length; y <-0 until regexNodes.length) yield (x, y)
    val elems = indices.filter{case(x, y)=> x!=y}
      .map{case(x, y)=> (regexNodes(x).toOrNodeIndex(regexNodes(y)))}
    val regexes = elems.map(nodeIndex=> nodeIndex.toRegex()).toSet
    regexes
  }*/

}

class RegexMultiString(val regexSearch: AbstractRegexSearch, filterRatio: Double = 0.0, count: Int = 20) extends RegexGenerator(filterRatio, count) {

  def generateTimely(): Set[String] = {
    System.out.println("Generating multi-regex...")
    val set = TimeBox.measureTime[Set[String]]("Regex-Multi", generate())
    filterTimely(set)
  }

  def filterTimely(set: Set[String]): Set[String] = {
    TimeBox.measureTime[Set[String]]("Regex-Multi", filter(set))
  }

  def generate(): Set[String] = {

    val paths = regexSearch.addPositive(positives)
      .addNegative(negatives)
      .searchDirectionalNegative()
      .sortBy(_.cost)
      .toArray

    val regexNodes = paths.map(crrPath => {
      crrPath.toOrRegex().constructRegexNode()
    }).distinct

    val indices = for (x <- 0 until regexNodes.length; y <- 0 until regexNodes.length) yield (x, y)
    val elems = indices.filter { case (x, y) => x != y }
      .map { case (x, y) => (regexNodes(x).combineOrNode(regexNodes(y))) }
    val regexes = elems.map(nodeIndex => nodeIndex.toRegex()).toSet

    if (regexNodes.isEmpty){
      Set()
    }
    else if (regexes.isEmpty) {
      Set(regexNodes.head.toRegex())
    }
    else {
      Set(regexes.head)
    }
  }

  def filter(set: Set[String]): Set[String] = {
    set.filter(regex => {
      val matchAll = positives.forall(positive => positive.matches(regex))
      val notMatchAll = negatives.forall(negative => negative.matches(regex))
      matchAll && notMatchAll
    })
  }
}

class NGramMultiRegex(val regexSearch: AbstractRegexSearch, filterRatio: Double = 0.0, count: Int = 20) extends RegexGenerator(filterRatio, count) {

  def generateTimely(): Set[String] = {
    System.out.println("Generating regex...")
    val set = TimeBox.measureTime[Set[String]]("NGram-Multi", generate())
    filterTimely(set)
  }

  override def generate(): Set[String] = {
    val sumPositive = positiveFilter.freqDictionary.map(_._2).sum
    val sumNegative = negativeFilter.freqDictionary.map(_._2).sum

    val positiveCases = positiveFilter.freqDictionary.filter { case (_, count) => {
      count.toDouble / sumPositive > 0.5
    }
    }.map(_._1).toSeq
    val negativeCases = negativeFilter.freqDictionary.filter { case (_, count) => {
      count.toDouble / sumNegative > 0.5
    }
    }.map(_._1).toSeq

    val paths = regexSearch.addPositive(positiveCases)
      .addNegative(negativeCases)
      .searchDirectionalNegative()
      .sortBy(_.cost)
      .toArray

    val regexNodes = paths.map(crrPath => {
      crrPath.toOrRegex().constructRegexNode()
    }).distinct

    val indices = for (x <- 0 until regexNodes.length; y <- 0 until regexNodes.length) yield (x, y)
    val elems = indices.filter { case (x, y) => x != y }
      .map { case (x, y) => (regexNodes(x).combineOrNode(regexNodes(y))) }
    val regexes = elems.map(nodeIndex => nodeIndex.toRegex()).toSet

    if(regexNodes.isEmpty){
      Set()
    }
    else if (regexes.isEmpty) {
      Set(regexNodes.head.toRegex())
    }
    else {
      Set(regexes.head)
    }
    regexes

  }

  def filterTimely(set: Set[String]): Set[String] = {
    TimeBox.measureTime[Set[String]]("NGram-Multi", filter(set))
  }

  def filter(set: Set[String]): Set[String] = {
    set.filter(regex => {
      val matchAll = positives.forall(positive => positive.matches(regex))
      val notMatchAll = negatives.forall(negative => negative.matches(regex))
      matchAll && notMatchAll
    })
  }
}




object RegexString {

  def applyExact(positives: Set[String]): RegexGenerator = {
    new RegexSingleString(new SinglePositiveExact(), TagExperimentCodes.patternFilterRatio, TagExperimentCodes.commonSampleCount).addPositives(positives)
  }

  def applyExactAdaptive(positives: Set[String]): RegexGenerator = {
    if (positives.head.length > 20) {}
    new RegexSingleString(new SinglePositiveExact(), TagExperimentCodes.patternFilterRatio, TagExperimentCodes.commonSampleCount).addPositives(positives)
  }

  def applyApproximate(positives: Set[String]): RegexGenerator = {
    new RegexSingleString(new SinglePositiveApprox(), TagExperimentCodes.patternFilterRatio, TagExperimentCodes.commonSampleCount).addPositives(positives)
  }

  def applyExact(positives: Set[String], negatives: Set[String]): RegexGenerator = {
    new RegexMultiString(new MultiPositiveExact(), TagExperimentCodes.patternFilterRatio, TagExperimentCodes.commonSampleCount).addPositives(positives).addNegatives(negatives)
  }

  def applyApproximate(positives: Set[String], negatives: Set[String]): RegexGenerator = {
    new RegexMultiString(new MultiPositiveApprox(), TagExperimentCodes.patternFilterRatio, TagExperimentCodes.commonSampleCount).addPositives(positives).addNegatives(negatives)
  }

  def applyExactNGram(positives: Set[String], negatives: Set[String]): RegexGenerator = {
    new NGramMultiRegex(new MultiPositiveExact(), TagExperimentCodes.patternFilterRatio, TagExperimentCodes.commonSampleCount).addPositives(positives).addNegatives(negatives)
  }

  def applyApproximateNGram(positives: Set[String], negatives: Set[String]): RegexGenerator = {
    new NGramMultiRegex(new MultiPositiveApprox(), TagExperimentCodes.patternFilterRatio, TagExperimentCodes.commonSampleCount).addPositives(positives).addNegatives(negatives)
  }

}
