package edu.btu.operands

import edu.btu.search.SinglePositiveExact

object RegexTest {

  def main(args: Array[String]): Unit = {
    test1()
  }
  def test0(): Unit ={
    val seq = Seq("abc","abz", "xyz")
    new RegexSingleString(new SinglePositiveExact()).testOrEfficient(seq, new SinglePositiveExact())
  }
  def test1(): Unit ={
    val seq = Seq("assets!", "horchow", "!horcho", "orchow!", "!images", "rchow!c", "1!produ", "!assets", "!com!ca", "!!image")
    new RegexSingleString(new SinglePositiveExact()).testOrEfficient(seq, new SinglePositiveExact())
  }

}
