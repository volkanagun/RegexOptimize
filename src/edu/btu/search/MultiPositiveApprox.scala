package edu.btu.search
import edu.btu.task.evaluation.TimeBox
import edu.btu.operands.{Cell, Path, RegexNode, RegexNodeIndex, Regexify}

import scala.util.control.Breaks

class MultiPositiveApprox() extends AbstractRegexSearch() {

  override def searchDirectional(): Seq[Path] = {
    TimeBox.measureTime[Seq[Path]]("approx-multi-positive", searchMultiDirectional(positives))
  }

  override def searchNegative(): Seq[Path] = {
    TimeBox.measureTime[Seq[Path]]("approx-multi-negative", searchMultiDirectional(negatives))
  }

  override def regexify(value: String): Seq[RegexNodeIndex] = {
    Regexify.randomizedGrouping(value)
  }

  override def randomize(regexes: Set[String]): Set[String] = regexes.flatMap(regex=> Regexify.toRandom(regex))

}
