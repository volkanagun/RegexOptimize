package edu.btu.operands

import edu.btu.task.evaluation.{ExperimentParams, TimeBox}
import edu.btu.search.{AbstractRegexSearch, MultiPositiveApprox, MultiPositiveExact, NGramFilter, SinglePositiveApprox, SinglePositiveExact}
import scala.util.Random

class RegexSingleString(experimentParams: ExperimentParams, val regexSearch: AbstractRegexSearch, override val patternFilterRatio: Double = 0.0, val count: Int = 20) extends RegexGenerator(experimentParams, patternFilterRatio, count) {

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

    val regexNodes = paths.par.map(crrPath => {
      crrPath.toOrRegex().constructRegexNode()
    }).toArray

    val elems = combine(regexNodes, ExperimentParams.maxCombineSize, ExperimentParams.maxRegexSize)
    val regexes = elems.map(nodeIndex => nodeIndex.toRegex())
    val newRegex = regexSearch.randomize(regexes)

    newRegex
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

class RegexMultiString(experimentParams: ExperimentParams, val regexSearch: AbstractRegexSearch, filterRatio: Double = 0.0, count: Int = 20) extends RegexGenerator(experimentParams, filterRatio, count) {

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

  def apply(experimentParams: ExperimentParams, positives: Set[String], search: AbstractRegexSearch):RegexGenerator = {
    new RegexSingleString(experimentParams, search, ExperimentParams.patternFilterRatio, ExperimentParams.topCount).addPositives(positives)
  }

  def apply(experimentParams: ExperimentParams, positives: Set[String], negatives:Set[String], search: AbstractRegexSearch):RegexGenerator={
    new RegexMultiString(experimentParams, search, ExperimentParams.patternFilterRatio, ExperimentParams.topCount).addPositives(positives).addNegatives(negatives)
  }

  def applyExact(experimentParams: ExperimentParams, positives: Set[String]): RegexGenerator = {
    new RegexSingleString(experimentParams, new SinglePositiveExact(), ExperimentParams.patternFilterRatio, ExperimentParams.topCount).addPositives(positives)
  }

  def applyExactAdaptive(experimentParams: ExperimentParams, positives: Set[String]): RegexGenerator = {
    if (positives.head.length > 20) {}
    new RegexSingleString(experimentParams, new SinglePositiveExact(), ExperimentParams.patternFilterRatio, ExperimentParams.topCount).addPositives(positives)
  }

  def applyApproximate(experimentParams: ExperimentParams, positives: Set[String]): RegexGenerator = {
    new RegexSingleString(experimentParams, new SinglePositiveApprox(), ExperimentParams.patternFilterRatio, ExperimentParams.topCount).addPositives(positives)
  }

  def applyExact(experimentParams: ExperimentParams, positives: Set[String], negatives: Set[String]): RegexGenerator = {
    new RegexMultiString(experimentParams, new MultiPositiveExact(), ExperimentParams.patternFilterRatio, ExperimentParams.topCount).addPositives(positives).addNegatives(negatives)
  }

  def applyApproximate(experimentParams: ExperimentParams, positives: Set[String], negatives: Set[String]): RegexGenerator = {
    new RegexMultiString(experimentParams, new MultiPositiveApprox(), ExperimentParams.patternFilterRatio, ExperimentParams.topCount).addPositives(positives).addNegatives(negatives)
  }

  def applyNGram(experimentParams: ExperimentParams, positives: Set[String]): RegexGenerator = {
    new NGramSinglePattern(experimentParams, ExperimentParams.patternFilterRatio, ExperimentParams.topCount)
      .addPositives(positives)
  }

  def applyNGram(experimentParams: ExperimentParams, positives: Set[String], negatives: Set[String]): RegexGenerator = {
    new NGramMultiPattern(experimentParams, ExperimentParams.patternFilterRatio, ExperimentParams.topCount)
      .addPositives(positives)
      .addNegatives(negatives)
  }

}
