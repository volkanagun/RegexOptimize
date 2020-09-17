package edu.btu.task.evaluation

import java.io.File

object Main {


  val accepted = Array[String]()
  var topCounts = Array(5, 10, 20, 50, 100, 200, 400)
  val maxRegexSizes = Array(1, 2, 3, 5, 10, 20, 50)
  val maxCombineSizes = Array(3, 20, 50)
  val maxThresholds = Array(0.2, 0.5, 0.7, 0.9)
  val folder = "resources/evaluations/backupv1/"

  ExperimentParams.loadXML()
  ExperimentParams.ngramLength = 5
  ExperimentParams.matchSelectRatio = 0.8
  ExperimentParams.saveXML()

  def main(args: Array[String]): Unit = {

    //summarize(folder)
    multiExperiment(ExperimentParams.singleExact)

  }

  def singleExperiment(): Unit = {

    val mainFolder = ExperimentParams.datasets
    new TagExperiment().initParams()
      .evaluate(mainFolder)
      .summary()

  }

  def summarize(folder: String): Unit = {

    val list = new File(folder).listFiles().map(f => EvaluationResult(null).load(f.getAbsolutePath))
      .map(eval => {
        (eval.experimentParams.topCount, eval.experimentParams.maxPaths, eval)
      })
      .sortWith { case ((tcount1, psize1, _), (tcount2, psize2, _)) => {
        tcount1 < tcount2 || (tcount1 == tcount2 && psize1 < psize2)
      }
      }
      .map(_._3)

    println(s"Evaluation Folder ${folder}")
    list.foreach(eval => {
      println(s"Generation ID: ${eval.experimentParams.generationID()}")
      println(s"Top count: ${eval.experimentParams.topCount} and Path size: ${eval.experimentParams.maxPaths}")
      println(s"Precision: ${eval.precision} Recall: ${eval.recall}  Accuracy:${eval.accuracy}")
      println(s"Counts:\n${eval.ratioCount.map { case (name, value) => "--->" + name + ":" + value }.mkString("\n")}")
    })

  }

  def multiExperiment(name: String): Unit = {

    val mainFolder = ExperimentParams.datasets
    var percentage = 0.0

    var maxSize = maxRegexSizes.size * maxCombineSizes.size * maxThresholds.size * topCounts.size

    println(s"CURRENT PROGRESS : ${percentage / maxSize}")
    maxRegexSizes.foreach(maxRegexSize => {
      println(s"CURRENT PROGRESS : ${percentage / maxSize}")
      topCounts.foreach(topCount => {
        maxThresholds.foreach { threshold => {
          println(s"CURRENT PROGRESS : ${percentage / maxSize}")
          maxCombineSizes.par.foreach(maxCombineSize => {

            new TagExperiment()
              .initParams(name, topCount, maxRegexSize, maxCombineSize, threshold)
              .evaluateNecessary(mainFolder)
              .summary()

            percentage += 1
            println(s"CURRENT PROGRESS : ${percentage / maxSize}")

          })
        }
        }
      })
    })
  }
}
