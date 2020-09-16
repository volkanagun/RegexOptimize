package edu.btu.task.evaluation

import scala.util.Random

case class LabelResult(var label:String) extends Serializable {

  var tp = 0
  var fp = 0
  var tn = 0
  var fn = 0
  var cnt = 0;

  def append(labelResult: LabelResult): LabelResult = {
    this.tp += labelResult.tp
    this.fp += labelResult.fp
    this.tn += labelResult.tn
    this.fn += labelResult.fn
    this
  }

  def incrementTP():this.type = {
    tp += 1
    this
  }

  def incrementFP():this.type = {
    fp += 1
    this
  }

  def incrementTN():this.type = {
    tn += 1
    this
  }

  def incrementFN():this.type = {
    fn += 1
    this
  }

  def incrementCount():this.type = {
    cnt += 1
    this
  }

  def print(): Unit = {
    println(s"Label ${label}")
    println("\t\tTrue positive rate: " + tp.toDouble/cnt)
    println("\t\tFalse positive rate: " + fp.toDouble/cnt)
    println("\t\tTrue negative rate: " + tn.toDouble/cnt)
    println("\t\tFalse negative rate: " + fn.toDouble/cnt)
  }

}

case class EvalResult() extends Serializable {

  var map = Map[String, LabelResult]()

  def add(labelResult: LabelResult): EvalResult = {
    if(map.contains(labelResult.label)) map(labelResult.label).append(labelResult)
    else map = map + (labelResult.label -> labelResult)
    this
  }

  def print(): Unit = {
    map.foreach{case(value, result) => {
      println("========================")
      result.print()
      println("========================")
    }}
  }
}


object RegexEvaluate {

  val csvfolder = "resources/img-csvs/"
  val takeTopPaths = 2


  def kfoldPartition(sequence:Seq[(String, Boolean)], fold:Int) : Seq[(Seq[(String, Boolean)], Seq[(String, Boolean)])] = {

    val rndSeq = Random.shuffle(sequence.toList)
    var crrPos = rndSeq.filter(_._2)
    var crrNeg = rndSeq.filter(! _._2)

    var finalSeq = Seq[(Seq[(String, Boolean)], Seq[(String, Boolean)])]()
    val posSize = math.ceil(crrPos.length.toDouble / fold).toInt
    val negSize = math.ceil(crrNeg.length.toDouble / fold).toInt
    var resPos = crrPos
    var resNeg = crrNeg

    for(k <- 0 until fold) {

      val crrPosTest = crrPos.take(posSize.toInt)
      val crrNegTest = crrNeg.take(negSize.toInt)

      val testSeq = crrPosTest ++ crrNegTest
      val trainSeq = resPos.slice(posSize.toInt, resPos.length) ++ resNeg.slice(negSize.toInt, resNeg.length)

      crrPos = crrPos.slice(posSize.toInt, crrPos.length)
      crrNeg = crrNeg.slice(negSize.toInt, crrNeg.length)

      resPos = resPos.slice(posSize.toInt, resPos.length) ++ crrPosTest
      resNeg = resNeg.slice(negSize.toInt, resNeg.length) ++ crrNegTest

      finalSeq = finalSeq :+ (trainSeq, testSeq)
    }

    finalSeq
  }

  def evaluate(sequence:Seq[(String, Boolean)], approach:(Seq[(String, Boolean)], Int) => Seq[String], kfold:Int, approachType:Int) : EvalResult = {

    val partitions = kfoldPartition(sequence, kfold)
    val evalResult = EvalResult()

    partitions.foreach(partition => {
      val trainingSet = partition._1
      val testingSet = partition._2
      val regsex  = approach(trainingSet, approachType)
      testingSet.foreach{case(text, posNeg) => {
        val label = if(posNeg) "Positive" else "Negative"
        evalResult.add(test(label, regsex, text))
      }}
    })

    evalResult
  }

  def test(label:String, regsex:Seq[String], text:String) : LabelResult = {
    val labelResult = LabelResult(label)
    println(s"Number of test regexes: ${regsex.length}")

    regsex.foreach(regex => {
      val doMatch = text.matches("^" + regex + "$")
      if("Positive".matches(label) && doMatch) labelResult.incrementTP().incrementCount()
      else if("Positive".matches(label) && !doMatch) labelResult.incrementFP().incrementCount()
      else if("Negative".matches(label) && doMatch) labelResult.incrementFN().incrementCount()
      else if("Negative".matches(label) && !doMatch) labelResult.incrementTN().incrementCount()
    })

    labelResult
  }

  /*def approach(sequence:Seq[(String,Boolean)], extractType:Int) : Seq[String] = {
    val regexes = if(extractType == 0) approach0(sequence)
    else if(extractType == 1) {Seq()}
    else if(extractType == 2) {Seq()}
    else {Seq()}
    regexes
  }*/

  /**
   * Binary merge matrix of all
   * Positive single and non-grouping scenario
   *
   * @param seq
   */

  /*def approach0(seq: Seq[(String, Boolean)]): Seq[String] = {

    val positiveCases = seq.filter(_._2).map(_._1)
    val negativeCases = seq.filter(!_._2).map(_._1)

    val regexSearch = new MultiPositiveExact()
      .addPositive(positiveCases)

    val matrices = regexSearch.search()
    val paths = regexSearch.searchLoop(matrices, takeTopPaths)

    val regexes = paths.flatten.flatMap(crrPath => {
      crrPath.toExactRegex()
    }).distinct

    val positiveFilter = regexes.filter(regex => {
      positiveCases.forall(item => item.matches("^" + regex + "$"))
    })

    val negativeFilter = regexes.filter(regex => {
      negativeCases.forall(item => !item.matches("^" + regex + "$"))
    })

    positiveFilter.intersect(negativeFilter)
  }*/



 /* def main(args: Array[String]): Unit = {
    val dataset = PatternReader.readCSV(csvfolder, 0.001)
    val result = evaluate(dataset, approach, 5, 0)
    result.print()
  }*/


}
