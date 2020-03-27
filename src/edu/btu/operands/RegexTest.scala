package edu.btu.operands

import edu.btu.search.{AbstractRegexSearch, MultiPositiveApprox, MultiPositiveExact, SinglePositiveApprox, SinglePositiveExact}

object RegexTest {

  def main(args: Array[String]): Unit = {
    test9(3)
  }

  def method(i: Int): AbstractRegexSearch = {
    if (i == 0) new SinglePositiveExact()
    else if (i == 1) new SinglePositiveApprox()
    else if (i == 2) new MultiPositiveExact()
    else if (i == 3) new MultiPositiveApprox()
    else null
  }

  def test0(methodIndex: Int): Unit = {
    val sequence1 = "Phone: 34576890989";
    val sequence2 = "Phone: 24585645464";
    val sequence3 = "Phone: 54585645464";

    test(Seq(sequence1, sequence2, sequence3), method(methodIndex), 0)
  }

  def test1(methodIndex: Int): Unit = {

    val sequence1 = "345 PH+";
    val sequence2 = "!45 PH-";
    test(Seq(sequence1, sequence2), method(methodIndex), 0)
  }

  def test2(methodIndex: Int): Unit = {
    val sequence1 = "5abc5:";
    val sequence2 = "pb5:4";
    test(Seq(sequence1, sequence2), method(methodIndex), 0)
  }

  def test3(methodIndex: Int): Unit = {
    val sequence1 = "abcd5";
    val sequence2 = "axyzp10";
    test(Seq(sequence1, sequence2), method(methodIndex), 0)
  }

  def test4(methodIndex: Int): Unit = {
    val sequence1 = "0A?p7p8";
    val sequence2 = "0123A4p?";
    test(Seq(sequence1, sequence2), method(methodIndex), 0)
  }

  def test5(methodIndex: Int): Unit = {
    val sequence1 = "abcd";
    val sequence2 = "axd";
    val sequence3 = "y";

    test(Seq(sequence1, sequence2, sequence3), method(methodIndex), 0)
  }

  def test6(methodIndex: Int): Unit = {
    val sequence1 = "xzabcx";
    val sequence2 = "xyefgx";
    val sequence3 = "xabcdx";
    test(Seq(sequence1, sequence2, sequence3), method(methodIndex), 0)
  }

  def test7(methodIndex: Int): Unit = {
    val sequence1 = "xyzabc";
    val sequence2 = "abc";
    /* val sequence3 = "prbabc";
     val sequence4 = "tlmabc";e*/

    test(Seq(sequence1, sequence2 /*, sequence3, sequence4*/), method(methodIndex), 0)
  }

  def test8(methodIndex: Int): Unit = {
    val sequence1 = "xyzabc";
    val sequence2 = "abc";
    /* val sequence3 = "prbabc";
     val sequence4 = "tlmabc";e*/

    test(Seq(sequence1, sequence2 /*, sequence3, sequence4*/), method(methodIndex), 3)
  }

  def test9(methodIndex: Int): Unit = {
    val positive0 = "567trt";
    val positive1 = "xyzabc";
    val positive2 = "abc";
    val negative1 = "xyz";
    val negative2 = "xyz";
    /* val sequence3 = "prbabc";
     val sequence4 = "tlmabc";e*/
    test(Seq(positive0, positive1, positive2), Seq(negative1), method(methodIndex), 3)
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


  def test1(): String = {
    val path = Path()

    path.addCell(cell(0, 0, "x", "x"), 0.0)
      .addCell(cell(0, 1, "x", "y"), 0)
      .addCell(cell(1, 1, "y", "y"), 0)
      .addCell(cell(2, 1, "z", "y"), 0)
      .addCell(cell(2, 2, "z", "z"), 0)
      .addCell(cell(3, 2, "a", "z"), 0)

    val param = path.toOrRegex()
    param.constructRegex()
  }

  def test2(): String = {
    val path = Path()

    path.addCell(cell(0, 0, "x", "x"), 0.0)
      .addCell(cell(1, 1, "y", "y"), 0)
      .addCell(cell(2, 2, "z", "z"), 0)
      .addCell(cell(3, 3, "a", "a"), 0)
      .addCell(cell(4, 4, "a", "b"), 0)
      .addCell(cell(4, 5, "a", "c"), 0)
      .addCell(cell(5, 5, "a", "c"), 0)

    val param = path.toOrRegex()
    val regexStr = param.constructRegex()
    println(regexStr)
    regexStr

  }

  def test3(): String = {
    val path = Path()

    path.addCell(cell(0, 0, "x", "x"), 0.0)
      .addCell(cell(1, 1, "y", "y"), 0)
      .addCell(cell(2, 2, "z", "z"), 0)
      .addCell(cell(2, 3, "z", "a"), 0)
      .addCell(cell(2, 4, "z", "b"), 0)
      .addCell(cell(2, 5, "z", "c"), 0)

    val param = path.toOrRegex()
    val regexStr = param.constructRegex()
    println(regexStr)
    regexStr

  }

  def test4(): String = {
    val path = Path()

    path.addCell(cell(0, 0, "a", "x"), 0.0)
      .addCell(cell(0, 1, "a", "y"), 0)
      .addCell(cell(0, 2, "a", "z"), 0)
      .addCell(cell(0, 3, "a", "a"), 0)
      .addCell(cell(1, 4, "b", "b"), 0)
      .addCell(cell(2, 5, "c", "c"), 0)

    val param = path.toOrRegex()
    val regexStr = param.constructRegex()
    println(regexStr)
    regexStr

  }

  def test5(): String = {
    val path = Path()

    path.addCell(cell(0, 0, "a", "x"), 0.0)
      .addCell(cell(1, 0, "b", "x"), 0)
      .addCell(cell(2, 0, "c", "x"), 0)
      .addCell(cell(3, 0, "x", "x"), 0)
      .addCell(cell(4, 1, "y", "y"), 0)
      .addCell(cell(5, 2, "z", "z"), 0)
      .addCell(cell(6, 2, "a", "z"), 0)
      .addCell(cell(7, 2, "b", "z"), 0)
      .addCell(cell(8, 2, "c", "z"), 0)

    val param = path.toOrRegex()
    val regexStr = param.updateRegex()
    println(regexStr)
    regexStr

  }

  def test6(): String = {
    val path = Path()

    path.addCell(cell(0, 0, "a", "x"), 0.0)
      .addCell(cell(1, 0, "b", "x"), 0)
      .addCell(cell(2, 0, "c", "x"), 0)
      .addCell(cell(3, 0, "x", "x"), 0)
      .addCell(cell(4, 1, "y", "y"), 0)
      .addCell(cell(5, 2, "z", "z"), 0)
      .addCell(cell(6, 3, "a", "a"), 0)
      .addCell(cell(7, 4, "b", "b"), 0)
      .addCell(cell(8, 5, "c", "c"), 0)

    val param = path.toOrRegex()
    val regexStr = param.updateRegex()
    println(regexStr)
    regexStr

  }

  def test7(): String = {
    val path = Path()

    path.addCell(cell(0, 0, "a", "x"), 0.0)
      .addCell(cell(1, 0, "b", "x"), 0)
      .addCell(cell(2, 0, "c", "x"), 0)
      .addCell(cell(3, 0, "x", "x"), 0)
      .addCell(cell(4, 1, "y", "y"), 0)
      .addCell(cell(5, 2, "z", "z"), 0)
      .addCell(cell(6, 0, "a", "x"), 0)
      .addCell(cell(7, 1, "b", "y"), 0)
      .addCell(cell(8, 2, "c", "z"), 0)

    val param = path.toOrRegex()
    val regexStr = param.updateRegex()
    println(regexStr)
    regexStr

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

    val regexNodes = paths.map(crrPath => {
      crrPath.toOrRegex().constructRegexNode()
    }).distinct

    val indices = for(x<-0 until regexNodes.length; y <-0 until regexNodes.length) yield (x, y)
    val elems = indices.filter{case(x, y)=> x!=y}
      .map{case(x, y)=> (regexNodes(x).toOrNodeIndex(regexNodes(y)))}
    val regexes = elems.map(nodeIndex=> nodeIndex.toRegex())

    matchTest(regexes, sequences)

  }

  def testOrEfficient(positives: Seq[String], negatives: Seq[String], regexSearch: AbstractRegexSearch): Unit = {
    val paths = regexSearch.addPositive(positives).addNegative(negatives).searchDirectionalNegative()
      .sortBy(_.cost)
      .toArray

    val regexes = paths.map(crrPath => {
      crrPath.toOrRegex().updateRegex()
    }).distinct

    matchTest(regexes, positives)
  }


  def testZigzag(sequences: Seq[String], regexSearch: AbstractRegexSearch): Unit = {
    val matrices = regexSearch.addPositive(sequences).search()
    val paths = regexSearch.searchZigZagLoop(matrices, 10)

    val regexes = paths.flatten.flatMap(crrPath => {
      crrPath.toRegex()
    }).distinct

    matchTest(regexes, sequences)

  }


  def matchTest(regexes: Seq[String], sequences: Seq[String]): Unit = {

    println("Regular expression count: " + regexes.length)

    regexes.foreach(regex => {
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
