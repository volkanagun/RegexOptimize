package edu.btu.task.tagmatch

object Main {

  def main(args: Array[String]): Unit = {

    val mainFolder = TagExperimentCodes.datasets

    new TagExperiment()
      .evaluate(mainFolder)
      .summary()

  }
}
