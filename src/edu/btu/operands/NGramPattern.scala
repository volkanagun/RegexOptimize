package edu.btu.operands

import java.util.regex.Pattern

import edu.btu.task.evaluation.{ExperimentParams, TimeBox}

/**
 * Use only positive cases
 *
 * @param filterRatio
 * @param count
 */
class NGramSinglePattern(experimentParams: ExperimentParams, filterRatio: Double = 0.0, count: Int = 20) extends RegexGenerator(experimentParams, filterRatio, count) {

  def generateTimely(): Set[String] = {
    System.out.println("Generating regex...")
    val set = TimeBox.measureTime[Set[String]]("generating-ngram-single", generate())
    filterTimely(set)
  }

  override def generate(): Set[String] = {
    positiveFilter.freqDictionary
      .toSeq.sortBy(_._2).reverse.map(_._1).toSet.take(count)
  }

  def generateOrRegex(nominalCases:Seq[String], preOp:String = ""):Set[String]={
    var mainSet = Set[String]()
    var inputSet = nominalCases
    for(i<-0 until ExperimentParams.maxRegexSize){
      var mainRegex = inputSet.head
      inputSet = inputSet.tail
      for(j<-0 until Math.min(ExperimentParams.maxCombineSize, inputSet.length)){
        mainRegex += "|"+inputSet(j)
      }
      mainSet += "("+preOp + mainRegex +")"
    }

    mainSet

  }

  def filterTimely(set: Set[String]): Set[String] = {
    TimeBox.measureTime[Set[String]]("filtering-ngram-single", filter(set))
  }

  def filter(set: Set[String]): Set[String] = {
    val posSet = filterMatch(set, positives)
    posSet
  }
}

/**
 * Use positive and negative cases
 * @param filterRatio
 * @param count
 */
class NGramMultiPattern(experimentParams: ExperimentParams, filterRatio: Double = 0.0, count: Int = 20) extends RegexGenerator(experimentParams, filterRatio, count) {

  def generateTimely(): Set[String] = {
    System.out.println("Generating regex...")
    val set = TimeBox.measureTime[Set[String]]("generating-ngram-multi", generate())
    filterTimely(set)
  }

  override def generate(): Set[String] = {
    val positiveCases = positiveFilter.freqDictionary.toSeq.sortBy(_._2).reverse.take(count)
      .map(_._1).map(item=> positiveFilter.clean(item))
/*
    val errors = testIf(positiveCases.toSet)
    if(!errors.isEmpty){
      val d = 0;
    }*/
    positiveCases.toSet
  }

  def testIf(positiveSet:Set[String]):Set[String]={
    val errorSet = positiveSet.filter(item=> {
      try{
        Pattern.compile(item)
        false
      }
      catch{
        case _ => true
      }
    })

    errorSet

  }

  def generateOrRegex(nominalCases:Seq[String], preOp:String = ""):Set[String]={
    var mainSet = Set[String]()
    var inputSet = nominalCases

    for(i<-0 until ExperimentParams.maxRegexSize) {

      var mainRegex = inputSet.head
      inputSet = inputSet.tail

      for(j<-0 until Math.min(ExperimentParams.maxCombineSize, inputSet.length)){
        mainRegex += "|"+inputSet(j)
      }

      mainSet += "("+preOp + mainRegex +")"
    }

    mainSet

  }

  def filterTimely(set: Set[String]): Set[String] = {
    TimeBox.measureTime[Set[String]]("filtering-ngram-multi", filter(set))
  }

  def filter(set: Set[String]): Set[String] = {
    val posSet = filterMatch(set, positives)
    val negSet = filterNotMatch(set, negatives)
    posSet.intersect(negSet)
  }
}
