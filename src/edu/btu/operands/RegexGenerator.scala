package edu.btu.operands

import edu.btu.search._
import edu.btu.task.evaluation.{EvaluationResult, ExperimentParams}

import scala.util.Random

abstract class RegexGenerator(val experimentParams: ExperimentParams, val patternFilterRatio: Double = 0.0, val topCount: Int = 20) extends Serializable {

  def generateTimely(): Set[String]

  def filter(): Boolean = {
    (!this.positives.isEmpty && !this.negatives.isEmpty) || (this.isInstanceOf[RegexSingleString] && positives.size >= 2 * experimentParams.k) ||
      (this.isInstanceOf[RegexMultiString] && this.positives.size >= 2 * experimentParams.k && this.negatives.size >= 2 * experimentParams.k)
  }

  def generate(): Set[String]

  var positives: Set[String] = Set()
  var negatives: Set[String] = Set()

  var positiveFilter = new NGramFilter(patternFilterRatio).setTopCount(topCount)
  var negativeFilter = new NGramFilter(patternFilterRatio).setTopCount(topCount)

  var trainingEval: EvaluationResult = EvaluationResult(null)

  def setTrainingEval(trainEval: EvaluationResult): this.type = {
    this.trainingEval = trainEval
    this
  }

  def addPositives(positives: Set[String]): this.type = {
    this.positives = this.positives ++ positives
    this.positiveFilter.count(positives)
    this
  }

  def addNegatives(negatives: Set[String]): this.type = {
    this.negatives = this.negatives ++ negatives
    this.negativeFilter.count(negatives)
    this
  }

  def filterSlice(): this.type = {
    this.negatives = this.negativeFilter.commonSet(positiveFilter.freqDictionary)
    this.positives = this.positiveFilter.commonSet(negativeFilter.freqDictionary)
    this
  }

  protected def combineBy(seq: Seq[RegexNodeIndex], size: Int, id: Int): RegexNodeIndex = {
    //take several combine if not have it
    var mainNode = Regexify.toOrNode(0)
    val shuffle = new Random().shuffle(seq).take(size)
    shuffle.foreach(rnode => if (!mainNode.contains(rnode)) mainNode = mainNode.combineOr(rnode))
    //new elements are added update hashcode
    mainNode.resetHash()
  }

  protected def combineBy(seq: Seq[RegexNodeIndex], size: Int, id: Int, doNegate: Boolean): RegexNodeIndex = {
    //take several combine if not have it
    var mainNode = Regexify.toOrNegateNode(0, Seq())
    val shuffle = new Random().shuffle(seq).take(size)
    shuffle.foreach(rnode => if (!mainNode.contains(rnode)) mainNode = mainNode.combineOr(rnode))
    //new elements are added update hashcode
    mainNode.resetHash()
  }

  protected def combineBy(crrSet: Set[RegexNodeIndex], newSet: Set[RegexNodeIndex]): this.type = {
    newSet.foreach(newNode => crrSet.filter(crrNode => !crrNode.contains(newNode))
      .foreach(crrNode => crrNode.combineOr(newNode)))
    this
  }

  def combine(seq: Seq[RegexNodeIndex], size: Int, repeat: Int = 3): Set[RegexNodeIndex] = {
    var crrSet = Set[RegexNodeIndex]()
    var newSet = seq.toSet

    for (k <- 0 until repeat) {
      crrSet = crrSet + combineBy(seq, size, experimentParams.shuffleSeed + k)
      combineBy(crrSet, newSet)
    }
    crrSet
  }

  def combineNegate(seq: Seq[RegexNodeIndex], size: Int, repeat: Int = 3): Set[RegexNodeIndex] = {
    var crrSet = Set[RegexNodeIndex]()
    var newSet = seq.toSet

    for (k <- 0 until repeat) {
      crrSet = crrSet + combineBy(seq, size, experimentParams.shuffleSeed + k, true)
      combineBy(crrSet, newSet)
    }
    crrSet
  }

  def isEmpty() = positives.isEmpty

  def longest(regexes: Set[String]): String = {
    regexes.toSeq.map(str => (str, str.length)).sortBy(_._2).reverse.head._1
  }

  def evalMatch(pcount: Set[(String, Int)], rsize: Int, psize: Int): Unit = {

    val countSum = pcount.map(_._2).sum.toDouble
    trainingEval.incRatio("positive-size", psize.toDouble)
    trainingEval.incRatio("positive-ratio", countSum / rsize)

  }

  def evalNotMatch(ncount: Set[(String, Int)], rsize: Int, psize: Int): Unit = {

    val countSum = ncount.map(_._2).sum.toDouble

    trainingEval.incRatio("negative-size", psize.toDouble)
    trainingEval.incRatio("negative-ratio", countSum / rsize)

  }

  def trainingSummary(): this.type = {
    trainingEval.trainingSummary()
    this
  }

  def filter(ridmap:Map[Int, Set[String]],regexArray:Array[String], trainingSamples:Set[String]):(Set[String], Set[String])={
    val arr = ridmap.flatMap(crr => ridmap.map(nxt => (crr, nxt))).filter { case (p1, p2) => p1._1 != p2._1 }.map { case (p1, p2) => {
      (p1._1, p2._1, p1._2.union(p2._2))
    }
    }.toArray.sortBy(_._3.size)

    if (arr.isEmpty) (Set(), trainingSamples)
    else {
      val (id1, id2, set) = arr.last
      (Set(regexArray(id1), regexArray(id2)), trainingSamples -- set)
    }
  }

  def complementaryMatch(regexSet: Set[String], trainingSamples: Set[String]): (Set[String], Set[String]) = {
    val regexArray = regexSet.toArray
    val ridmap = regexArray.zipWithIndex.map { case (regex, rid) => (rid, trainingSamples.filter(str => str.matches(regex))) }.toMap
    filter(ridmap, regexArray, trainingSamples)
  }

  def complementaryNotMatch(regexSet: Set[String], trainingSamples: Set[String]): (Set[String], Set[String]) = {
    val regexArray = regexSet.toArray
    val ridmap = regexArray.zipWithIndex.map { case (regex, rid) => (rid, trainingSamples.filter(str => !str.matches(regex))) }.toMap
    filter(ridmap, regexArray, trainingSamples)
  }

  //compute tp/fp/tn/fn for each regex
  def filterMatch(regexSet: Set[String], trainingSamples: Set[String]): Set[String] = {
    /*val counts = regexSet.map(regex=> (regex, trainingSamples.count(value=> value.matches(regex))))
    val sum = trainingSamples.size

    val fregexes = counts.filter{case(regex,cnt)=> cnt.toDouble/sum >= experimentParams.matchSelectRatio}
    evalMatch(fregexes,regexSet.size, trainingSamples.size)
    fregexes.map(_._1)*/

    var mainSet = Set[String]()
    var crrSamples = trainingSamples
    var crrRegexes = regexSet
    var ratio = 0.0
    var maxSize = trainingSamples.size
    var mycontinue = true
    while (!crrSamples.isEmpty && !crrRegexes.isEmpty && ratio < experimentParams.matchSelectRatio && mycontinue) {

      val (nextSet, nextSamples) = complementaryMatch(crrRegexes, crrSamples)
      if (crrSamples.size != nextSamples.size) {
        ratio += (crrSamples.size - nextSamples.size).toDouble / maxSize
        mainSet ++= nextSet
        crrRegexes = crrRegexes -- nextSet
        crrSamples = nextSamples
      }
      else{
        mycontinue = false
      }

    }

    mainSet
  }

  def filterNotMatch(regexSet: Set[String], trainingSamples: Set[String]): Set[String] = {
    /* val counts = regexSet.map(regex=> (regex, trainingSamples.count(value=> !value.matches(regex))))
     val sum = counts.map(_._2).sum

     val fregexes = counts.filter{case(regex,cnt)=> cnt.toDouble/sum >  experimentParams.matchSelectRatio}
     evalNotMatch(fregexes,regexSet.size, trainingSamples.size)
     fregexes.map(_._1)*/

    var mainSet = Set[String]()
    var crrSamples = trainingSamples
    var crrRegexes = regexSet
    var ratio = 0.0
    var maxSize = trainingSamples.size
    var mycontinue = true

    while (!crrSamples.isEmpty && !crrRegexes.isEmpty && ratio < experimentParams.matchSelectRatio && mycontinue) {
      val (nextSet, nextSamples) = complementaryNotMatch(crrRegexes, crrSamples)
      if (crrSamples.size != nextSamples.size) {

        ratio += (crrSamples.size - nextSamples.size).toDouble / maxSize
        mainSet ++= nextSet
        crrRegexes = crrRegexes -- nextSet
        crrSamples = nextSamples
      }
      else{
        mycontinue = false
      }

    }

    mainSet

  }

  //region Description
  //deneme
  //deneme
  //deneme
  //endregion

  def method(i: Int): AbstractRegexSearch = {

    if (i == 0) new SinglePositiveExact()
    else if (i == 1) new SinglePositiveApprox()
    else if (i == 2) new MultiPositiveExact()
    else if (i == 3) new MultiPositiveApprox()
    else null

  }


  def test(positives: Seq[String], negatives: Seq[String], regexSearch: AbstractRegexSearch, testIndex: Int): Unit = {
    if (testIndex == 3) testOrEfficient(positives, negatives, regexSearch)
  }

  def node(value: String, i: Int): RegexNodeIndex = RegexNodeIndex(i, RegexOp(Regexify.seq), Seq())
    .setMatchTxt(value)
    .setMatchGroup(value)
    .setMatchValue(value)

  def cell(i: Int, j: Int, src: String, dst: String): Cell = Cell(i, j, node(src, i), node(dst, j))


  def cellFromString(i: Int, j: Int, src: String, dst: String): Cell = {

    val nsrc = src(i).toString
    val ndst = dst(j).toString
    Cell(i, j, node(nsrc, i), node(ndst, j))

  }


  /*def testRegular(sequences: Seq[String], regexSearch: AbstractRegexSearch): Unit = {
    val matrices = regexSearch.addPositive(sequences).search()

    val paths = regexSearch.searchZigZagLoop(matrices, 10)
      .flatten
      .sortBy(_.cost)
      .toArray

    val regexes = paths.flatMap(crrPath => {
      crrPath.toRegex()
    }).distinct

    matchTest(regexes, sequences)
  }*/


  /*def testEfficient(sequences: Seq[String], regexSearch: AbstractRegexSearch): Unit = {
    val matrices = regexSearch.addPositive(sequences).search()
    val paths = regexSearch.search(matrices).flatten
      .sortBy(_.cost)
      .toArray

    val regexes = paths.flatMap(crrPath => {
      crrPath.toRegex()
    }).distinct

    matchTest(regexes, sequences)
  }*/

  def testOrEfficient(sequences: Seq[String], regexSearch: AbstractRegexSearch): Unit = {
    val regexes = RegexString.apply(experimentParams, sequences.toSet, regexSearch).generate().toSeq
    matchTest(regexes, sequences)
  }

  def testOrEfficient(positives: Seq[String], negatives: Seq[String], regexSearch: AbstractRegexSearch): Unit = {

    val regexes = RegexString.apply(experimentParams, positives.toSet, negatives.toSet, regexSearch).generate().toSeq
    matchTest(regexes, positives)
    matchTest(regexes, negatives, false)
  }


  /*def testZigzag(sequences: Seq[String], regexSearch: AbstractRegexSearch): Unit = {
    val matrices = regexSearch.addPositive(sequences).search()
    val paths = regexSearch.searchZigZagLoop(matrices, 10)

    val regexes = paths.flatten.flatMap(crrPath => {
      crrPath.toRegex()
    }).distinct

    matchTest(regexes, sequences)

  }*/


  def matchTest(regexes: Seq[String], sequences: Seq[String], positiveMatch: Boolean = true): Unit = {

    println("Regular expression count: " + regexes.length)
    var count = Map[String, Int]()

    regexes.foreach(regex => {
      println(s"Should match all : ${positiveMatch}")
      sequences.foreach(sequence => {
        if (!sequence.matches("^" + regex + "$")) {
          println(s"No - match: ${sequence} and regex:${regex}")
        }
        else {
          count = count.updated(sequence, count.getOrElse(sequence, 1))
          println(s"Yes - match: ${sequence} and regex:${regex}")
        }
      })
    })

    println(s"Match ratio: ${count.map(_._2).sum.toDouble / sequences.length}")

  }


}
