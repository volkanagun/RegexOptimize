package edu.btu.operands

import edu.btu.search.{AbstractRegexSearch, MultiPositiveApprox, MultiPositiveExact, NGramFilter, SinglePositiveApprox, SinglePositiveExact}

abstract class RegexGenerator(val filterRatio: Double = 0.0, val topCount: Int = 20) {

  def generateTimely(): Set[String]

  def generate(): Set[String]

  var positives: Set[String] = Set()
  var negatives: Set[String] = Set()

  var positiveFilter = new NGramFilter(filterRatio).setTopCount(topCount)
  var negativeFilter = new NGramFilter(filterRatio).setTopCount(topCount)

  def addPositives(positives: Set[String]): this.type = {
    this.positives = this.positives ++ positives
    this.positives = this.positiveFilter.count(this.positives).filter(this.positives)
    this
  }

  def addNegatives(negatives: Set[String]): this.type = {
    this.negatives = this.negatives ++ negatives
    this.negatives = this.negativeFilter.count(this.negatives).filter(this.negatives)
    this
  }

  def filterSlice(): this.type = {
    this.negatives = this.negativeFilter.filterSlice(negatives)
    this.positives = this.positiveFilter.filterSlice(positives)
    this
  }

  def isEmpty() = positives.isEmpty

  def longest(regexes: Set[String]): String = {
    regexes.toSeq.map(str => (str, str.length)).sortBy(_._2).reverse.head._1
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

  def test(sequences: Seq[String], regexSearch: AbstractRegexSearch, testIndex: Int): Unit = {

    if (testIndex == 0) testRegular(sequences, regexSearch)
    else if (testIndex == 1) testZigzag(sequences, regexSearch)
    else if (testIndex == 2) testEfficient(sequences, regexSearch)
    else if (testIndex == 3) testOrEfficient(sequences, regexSearch)

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


  def testRegular(sequences: Seq[String], regexSearch: AbstractRegexSearch): Unit = {
    val matrices = regexSearch.addPositive(sequences).search()

    val paths = regexSearch.searchZigZagLoop(matrices, 10)
      .flatten
      .sortBy(_.cost)
      .toArray

    val regexes = paths.flatMap(crrPath => {
      crrPath.toRegex()
    }).distinct

    matchTest(regexes, sequences)
  }


  def testEfficient(sequences: Seq[String], regexSearch: AbstractRegexSearch): Unit = {
    val matrices = regexSearch.addPositive(sequences).search()
    val paths = regexSearch.search(matrices).flatten
      .sortBy(_.cost)
      .toArray

    val regexes = paths.flatMap(crrPath => {
      crrPath.toRegex()
    }).distinct

    matchTest(regexes, sequences)
  }

  def testOrEfficient(sequences: Seq[String], regexSearch: AbstractRegexSearch): Unit = {

    val paths = regexSearch.addPositive(sequences)
      .searchDirectional()
      .sortBy(_.cost)
      .toArray

    //correct
    val regexNodes = paths.map(crrPath => {
      val orNode = crrPath.toOrRegex()
      val newNode = orNode.constructRegexNode()
      newNode
    }).distinct

    //fix here and toOrNodeIndex
    val indices = for (x <- 0 until regexNodes.length; y <- x+1 until regexNodes.length) yield (x, y)
    //multiple regexes with minimum length
    val elems =  indices.map { case (x, y) => (regexNodes(x).combineOrNode(regexNodes(y))) }
    val regexes = elems.map(nodeIndex => nodeIndex.toRegex())

    matchTest(regexes, sequences)

  }

  def testOrEfficient(positives: Seq[String], negatives: Seq[String], regexSearch: AbstractRegexSearch): Unit = {
    val paths = regexSearch
      .addPositive(positives)
      .addNegative(negatives)
      .searchDirectionalNegative()
      .sortBy(_.cost)
      .toArray

    val regexes = paths.map(crrPath => {
      crrPath.toOrRegex().updateRegex()
    }).distinct

    matchTest(regexes, positives)
    matchTest(regexes, negatives, false)
  }


  def testZigzag(sequences: Seq[String], regexSearch: AbstractRegexSearch): Unit = {
    val matrices = regexSearch.addPositive(sequences).search()
    val paths = regexSearch.searchZigZagLoop(matrices, 10)

    val regexes = paths.flatten.flatMap(crrPath => {
      crrPath.toRegex()
    }).distinct

    matchTest(regexes, sequences)

  }


  def matchTest(regexes: Seq[String], sequences: Seq[String], positiveMatch: Boolean = true): Unit = {

    println("Regular expression count: " + regexes.length)
    regexes.foreach(regex => {
      println(s"Should match all : ${positiveMatch}")
      sequences.foreach(sequence => {
        if (!sequence.matches("^" + regex + "$")) {
          println(s"No - match: ${sequence} and regex:${regex}")
        }
        else {
          println(s"Yes - match: ${sequence} and regex:${regex}")
        }
      })
    })
  }


}
