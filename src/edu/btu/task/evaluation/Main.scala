package edu.btu.task.evaluation

object Main {

  def main(args: Array[String]): Unit = {

    val mainFolder = ExperimentParams.datasets
    new TagExperiment().initParams()
      .evaluate(mainFolder)
      .summary()

  }
}
