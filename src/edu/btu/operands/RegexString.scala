package edu.btu.operands

import edu.btu.search.{AbstractRegexSearch, MultiPositiveApprox, MultiPositiveExact, NGramFilter, SinglePositiveApprox, SinglePositiveExact}
import edu.btu.task.tagmatch.TimeBox

import scala.util.Random

class RegexSingleString(val regexSearch:AbstractRegexSearch, val ratio:Double=0.0) extends RegexGenerator(ratio) {

  def filter(set:Set[String]):Set[String] = {
    set.filter(regex=> {
      val matchAll = positives.forall(positive=> positive.matches(regex))
      matchAll
    })
  }

  def generateTimely():Set[String]={
    System.out.println("Generating regex...")
    val set = TimeBox.measureTime[Set[String]]("Regex-Single", generate())
    filterTimely(set)
  }

  def filterTimely(set:Set[String]):Set[String]={
    TimeBox.measureTime[Set[String]]("Regex-Single-Filter",filter(set))
  }

  def generate():Set[String] = {

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

class RegexMultiString(val regexSearch:AbstractRegexSearch, filterRatio:Double = 0.0) extends RegexGenerator(filterRatio) {

  def generateTimely():Set[String]={
    System.out.println("Generating regex...")
    val set = TimeBox.measureTime[Set[String]]("Regex-Multi",generate())
    filterTimely(set)
  }

  def filterTimely(set:Set[String]):Set[String]={
    TimeBox.measureTime[Set[String]]("Regex-Multi",filter(set))
  }

  def generate() : Set[String] = {

    val paths = regexSearch.addPositive(positives)
      .addNegative(negatives)
      .searchDirectionalNegative()
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

  }

  def filter(set:Set[String]):Set[String] = {
    set.filter(regex=> {
      val matchAll = positives.forall(positive=> positive.matches(regex))
      val notMatchAll = negatives.forall(negative => negative.matches(regex))
      matchAll && notMatchAll
    })
  }
}

object RegexString{

  def applyExact(positives:Set[String]):RegexGenerator={
    new RegexSingleString(new SinglePositiveExact()).addPositives(positives)
  }

  def applyExactAdaptive(positives:Set[String]):RegexGenerator={
    if(positives.head.length > 20){}
    new RegexSingleString(new SinglePositiveExact()).addPositives(positives)
  }

  def applyApproximate(positives:Set[String]):RegexGenerator={
    new RegexSingleString( new SinglePositiveApprox()).addPositives(positives)
  }

  def applyExact(positives:Set[String], negatives:Set[String]):RegexGenerator={
    new RegexMultiString(new MultiPositiveExact()).addPositives(positives).addNegatives(negatives)
  }

  def applyApproximate(positives:Set[String], negatives:Set[String]):RegexGenerator={
    new RegexMultiString(new MultiPositiveApprox()).addPositives(positives).addNegatives(negatives)
  }

}
