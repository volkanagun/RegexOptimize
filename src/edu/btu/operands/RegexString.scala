package edu.btu.operands

import edu.btu.task.evaluation.{ExperimentParams, TimeBox}
import edu.btu.search.{AbstractRegexSearch, MultiPositiveApprox, MultiPositiveExact, NGramFilter, SinglePositiveApprox, SinglePositiveExact}


import scala.util.Random

class RegexSingleString(val regexSearch: AbstractRegexSearch, override val patternFilterRatio: Double = 0.0, val count: Int = 20) extends RegexGenerator(patternFilterRatio, count) {

  def filter(set: Set[String]): Set[String] = {
    filterMatch(set, positives)
  }

  def generateTimely(): Set[String] = {
    System.out.println("Generating single-regex...")
    System.out.println(s"Number of samples :${positives.size}...")
    val set = TimeBox.measureTime[Set[String]]("generating-regex-single", generate())
    filterTimely(set)
  }

  def filterTimely(set: Set[String]): Set[String] = {
    TimeBox.measureTime[Set[String]]("filtering-regex-single", filter(set))
  }

  def generate(): Set[String] = {

    val paths = regexSearch.addPositive(positives)
      .sizeControl()
      .searchDirectional()
      .sortBy(_.cost)
      .toArray
      /*.take(ExperimentParams.maxPaths)*/

    val regexNodes = paths.map(crrPath => {
      crrPath.toOrRegex().constructRegexNode()
    })

    val elems = combine(regexNodes, ExperimentParams.maxCombineSize, ExperimentParams.maxRegexSize)
    val regexes = elems.map(nodeIndex => nodeIndex.toRegex())

    regexSearch.randomize(regexes)

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
    val set = TimeBox.measureTime[Set[String]]("generating-regex-multi", generate())
    filterTimely(set)
  }

  def filterTimely(set: Set[String]): Set[String] = {
    TimeBox.measureTime[Set[String]]("filtering-regex-multi", filter(set))
  }

  def generate(): Set[String] = {

    val regexes = regexSearch.addPositive(positives)
      .addNegative(negatives)
      .sizeControl()
      .searchNegativeRegex()
      .toSet

    regexes

  }

  def filter(set: Set[String]): Set[String] = {
    val posSet = filterMatch(set, positives)
    val negSet = filterNotMatch(set, negatives)
    posSet.intersect(negSet)
  }
}


object RegexString {

  def apply(positives: Set[String], search: AbstractRegexSearch):RegexGenerator={
    new RegexSingleString(search, ExperimentParams.patternFilterRatio, ExperimentParams.topCount).addPositives(positives)
  }


  def apply(positives: Set[String], negatives:Set[String], search: AbstractRegexSearch):RegexGenerator={
    new RegexMultiString(search, ExperimentParams.patternFilterRatio, ExperimentParams.topCount).addPositives(positives).addNegatives(negatives)
  }

  def applyExact(positives: Set[String]): RegexGenerator = {
    new RegexSingleString(new SinglePositiveExact(), ExperimentParams.patternFilterRatio, ExperimentParams.topCount).addPositives(positives)
  }

  def applyExactAdaptive(positives: Set[String]): RegexGenerator = {
    if (positives.head.length > 20) {}
    new RegexSingleString(new SinglePositiveExact(), ExperimentParams.patternFilterRatio, ExperimentParams.topCount).addPositives(positives)
  }

  def applyApproximate(positives: Set[String]): RegexGenerator = {
    new RegexSingleString(new SinglePositiveApprox(), ExperimentParams.patternFilterRatio, ExperimentParams.topCount).addPositives(positives)
  }

  def applyExact(positives: Set[String], negatives: Set[String]): RegexGenerator = {
    new RegexMultiString(new MultiPositiveExact(), ExperimentParams.patternFilterRatio, ExperimentParams.topCount).addPositives(positives).addNegatives(negatives)
  }

  def applyApproximate(positives: Set[String], negatives: Set[String]): RegexGenerator = {
    new RegexMultiString(new MultiPositiveApprox(), ExperimentParams.patternFilterRatio, ExperimentParams.topCount).addPositives(positives).addNegatives(negatives)
  }

  def applyNGram(positives: Set[String]): RegexGenerator = {
    new NGramSinglePattern(ExperimentParams.patternFilterRatio, ExperimentParams.topCount)
      .addPositives(positives)
  }

  def applyNGram(positives: Set[String], negatives: Set[String]): RegexGenerator = {
    new NGramMultiPattern(ExperimentParams.patternFilterRatio, ExperimentParams.topCount)
      .addPositives(positives)
      .addNegatives(negatives)
  }

}
