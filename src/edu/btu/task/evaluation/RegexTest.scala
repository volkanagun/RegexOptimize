package edu.btu.task.evaluation

import edu.btu.operands.{RegexMultiString, RegexSingleString}
import edu.btu.search.{MultiPositiveApprox, SinglePositiveApprox, SinglePositiveExact}

object RegexTest {

  def main(args: Array[String]): Unit = {
    test4()
  }
  def test0(): Unit ={
    val seq = Seq("abc","abz", "xyz")
    new RegexSingleString(new SinglePositiveExact()).testOrEfficient(seq, new SinglePositiveExact())
  }

  def test1(): Unit ={

    val seq = Seq("assets!", "horchow", "!horcho", "orchow!", "!images", "rchow!c", "1!produ", "!assets", "!com!ca", "!!image")
    new RegexSingleString(new SinglePositiveExact()).testOrEfficient(seq, new SinglePositiveExact())

  }

  def test2(): Unit = {

    val seq = Seq("assets!", "horchow", "!horcho", "orchow!", "!images", "rchow!c", "1!produ", "!assets", "!com!ca", "!!image")
    new RegexSingleString(new SinglePositiveExact()).testOrEfficient(seq, new SinglePositiveApprox())

  }


  def test3(): Unit = {

    val seqPos = Seq("ABCXEFG", "ABCEFG")
    val seqNeg = Seq("5555555", "111111")

    new RegexMultiString(new MultiPositiveApprox()).testOrEfficient(seqPos,seqNeg, new MultiPositiveApprox())

  }

  def test4(): Unit = {

    val seqPos = Seq("123-WXTX", "123-KXTX")

    new RegexSingleString(new SinglePositiveExact()).testOrEfficient(seqPos, new SinglePositiveExact())

  }

  //random generalization

  def testNN1(): Unit = {


  }

  def testNN2(): Unit = {


  }

}
