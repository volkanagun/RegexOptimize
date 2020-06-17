package edu.btu.operands

import edu.btu.search.SinglePositiveExact

object RegexTest {

  def main(args: Array[String]): Unit = {
    val seq = Seq("abc","abz", "xyz")
    new RegexSingleString(new SinglePositiveExact()).testOrEfficient(seq, new SinglePositiveExact())
  }

}
