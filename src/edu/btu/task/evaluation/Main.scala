package edu.btu.task.evaluation

object Main {

  def main(args: Array[String]): Unit = {
    multiExperiment(ExperimentParams.singleExact)
  }

  def singleExperiment(): Unit ={

    val mainFolder = ExperimentParams.datasets
    new TagExperiment().initParams()
      .evaluate(mainFolder)
      .summary()

  }

  def multiExperiment(name:String): Unit ={
    var topCounts = Array(30, 50, 100, 500, 1000)
    var maxCombineSize = 50
    val mainFolder = ExperimentParams.datasets

    topCounts.foreach(topCount=> {
      new TagExperiment().initParams(name, topCount, maxCombineSize)
        .evaluate(mainFolder)
        .summary()

    })

  }
}
