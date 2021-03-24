package edu.btu.search

import edu.btu.operands.{Path, RegexNodeIndex, Regexify}

class RandomizedSearch() extends AbstractRegexSearch() {

  val multiPositiveApprox = new MultiPositiveApprox()
  val singlePositiveApprox = new SinglePositiveApprox()

  override def addPositive(samples: Set[String]): RandomizedSearch.this.type = {
    multiPositiveApprox.setPositive(samples)
    singlePositiveApprox.setPositive(samples)
    this
  }

  override def addNegative(samples: Set[String]): RandomizedSearch.this.type = {
    multiPositiveApprox.setNegative(samples)
    singlePositiveApprox.setNegative(samples)
    this
  }

  override def searchDirectional(): Seq[Path] = {
    singlePositiveApprox.searchDirectional() ++ multiPositiveApprox.searchDirectional()
  }

  override def searchNegative(): Seq[Path] = {
    singlePositiveApprox.searchNegative() ++ multiPositiveApprox.searchNegative()
  }

  override def regexify(value: String): Seq[RegexNodeIndex] = Regexify.continousGrouping(value)

  override def randomize(value: Set[String]): Set[String] = {
    value.flatMap(regex => Regexify.toRandom(regex))
  }
}
