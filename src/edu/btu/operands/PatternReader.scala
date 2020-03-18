package edu.btu.operands

import java.io.File

import scala.io.Source
import scala.util.Random

object PatternReader {

  def readCSV(folder: String, rate: Double): Seq[(String, Boolean)] = {

    val elems = new File(folder).listFiles().flatMap(
      filename => Source.fromFile(filename, "UTF-8").getLines().map(line => {
        val lineElems = line.split("\\,")
        val lineLastBool = lineElems.last != 0
        val pair = (lineElems.head, lineLastBool)
        pair
      }))

    Random.shuffle(elems.toList).take((elems.length * rate).toInt)
  }


  def main(args: Array[String]): Unit = {

    var i = 0;

    for( i <- 0 until 10) {

    }

    System.out.println(i)
  }

}
