package edu.btu.task.evaluation

import java.io.File

object Main {

  case class EvalKey(count: Int, paths: Int, ratio: Double, foldername: String, eval: EvaluationResult) {
    override def hashCode(): Int = {
      var r = 3;
      r = r * 7 + count
      r = r * 7 + paths
      r = r * 7 + ratio.hashCode()
      r = r * 7 + eval.experimentParams.maxCombineSize
      r = r * 7 + eval.experimentParams.maxMultiDepth
      r
    }

    override def equals(obj: Any): Boolean = {
      if (obj.isInstanceOf[EvalKey]) {
        val robj = obj.asInstanceOf[EvalKey]
        val rmatchSelectRatio  = robj.eval.experimentParams.matchSelectRatio
        val rmaxCombineSize  = robj.eval.experimentParams.maxCombineSize
        val rmaxPaths  = robj.eval.experimentParams.maxPaths
        val rtopCount  = robj.eval.experimentParams.topCount
        //val rmultiDepth  = robj.eval.experimentParams.maxMultiDepth

        val cmatchSelectRatio  = eval.experimentParams.matchSelectRatio
        val cmaxCombineSize  = eval.experimentParams.maxCombineSize
        val cmaxPaths  = eval.experimentParams.maxPaths
        val ctopCount  = eval.experimentParams.topCount
        //val cmultiDepth  = eval.experimentParams.maxMultiDepth

        rmatchSelectRatio == cmatchSelectRatio &&
          rmaxCombineSize == cmaxCombineSize &&
          rmaxPaths == cmaxPaths &&
          rtopCount == ctopCount
      }
      else {
        false
      }
    }
  }

  val accepted = Array[String]()
  var topCounts = Array(5 /*, 10 , 20,50, 100, 200,500*/)
  val maxRegexSizes = Array(/*1, 2 , 3, 5, 10, */20/*, 50, 100*/)
  val maxCombineSizes = Array(/*1, 3 , */20/*, 50*/)
  val maxThresholds = Array(/*0.2, 0.5 , */0.6/*, 0.7, 0.9*/)
  val experimentParams = ExperimentParams.loadXML()

  experimentParams.ngramLength = 3
  experimentParams.maxNgramLength = 7

  experimentParams.matchSelectRatio = 0.9
  experimentParams.patternFilterRatio = 0.0
  experimentParams.maxCombineSize = 20
  experimentParams.maxRegexSize = 5
  experimentParams.topCount = 500
  experimentParams.maxMultiDepth = 3
  experimentParams.experimentCycle = Array(ExperimentParams.naiveBayes)
  experimentParams.saveXML()

  def main(args: Array[String]): Unit = {
    //multiExperimentNGram(ExperimentParams.multiNGRAM)
    //multiExperiment(ExperimentParams.singleApprox)
    //multiExperiment(ExperimentParams.singleExact)
    //multiExperiment(ExperimentParams.multiApprox)
    /*multiExperiment(ExperimentParams.multiExact)
    multiExperiment(ExperimentParams.multiNGRAM)
    multiExperiment(ExperimentParams.singleNGRAM)*/
    singleExperiment("naive-bayes")
  }

  def singleExperiment(): Unit = {
    val mainFolder = ExperimentParams.datasets
    new TagExperiment().initParams(experimentParams)
      .evaluate(mainFolder)
      .summary("resources/evaluations/")
  }

  def singleExperiment(name:String): Unit = {
    val mainFolder = ExperimentParams.datasets
    val realEvalFolder = "resources/evaluations/" + name.toLowerCase + "/";

    new TagExperiment().initParams(experimentParams)
      .evaluate(mainFolder)
      .summary(realEvalFolder)
  }


  def multiExperiment(name: String): Unit = {

    val mainFolder = ExperimentParams.datasets
    var percentage = 0.0
    val realEvalFolder = "resources/evaluations/" + name.toLowerCase + "/";

    var p1Array = maxRegexSizes.flatMap(r => maxCombineSizes.map(c => (r, c)))
    var p2Array = maxThresholds.flatMap(t => topCounts.map(c => (t, c)))
    val paramsArray = p1Array.flatMap(p1 => p2Array.map(p2 => (p1._1, p1._2, p2._1, p2._2)))

    var maxSize = maxRegexSizes.size * maxCombineSizes.size * maxThresholds.size * topCounts.size

    println(s"CURRENT PROGRESS : ${percentage / maxSize}")
    println(s"SIZE : ${maxSize}")

    paramsArray.foreach(param => {

      val (maxRegexSize, maxCombineSize, threshold, topCount) = param;

      println(s"TOP COUNT: ${topCount}")

      new TagExperiment()
        .initParams(name, topCount, maxRegexSize, maxCombineSize, threshold)
        .evaluateNecessary(realEvalFolder, mainFolder)
        .summary(realEvalFolder)

      percentage += 1
      println(s"CURRENT PROGRESS : ${percentage / maxSize}")

    })


    /*maxRegexSizes.foreach(maxRegexSize => {

      println(s"CURRENT PROGRESS : ${percentage / maxSize}")

      topCounts.foreach(topCount => {

        println(s"TOP COUNT: ${topCount}")

        maxThresholds.foreach { threshold => {

          println(s"CURRENT PROGRESS : ${percentage / maxSize}")

          maxCombineSizes.par.foreach(maxCombineSize => {
            println(s"Parameters:\n   MaxRegexSize:${maxRegexSize}\n   MaxCombineSize: ${maxCombineSize}\n   TopCount:${topCount}\n   Threshold:${threshold}")

            new TagExperiment()
              .initParams(name, topCount, maxRegexSize, maxCombineSize, threshold)
              .evaluateNecessary(realEvalFolder, mainFolder)
              .summary(realEvalFolder)

            percentage += 1
            println(s"CURRENT PROGRESS : ${percentage / maxSize}")
          })
        }
        }
      })
    })*/

  }

  def multiExperimentNGram(name: String): Unit = {

    val mainFolder = ExperimentParams.datasets
    val realEvalFolder = "resources/" + name + "/";
    var percentage = 0.0

    var maxSize = maxRegexSizes.size * maxCombineSizes.size * maxThresholds.size * topCounts.size

    println(s"CURRENT PROGRESS : ${percentage / maxSize}")
    println(s"SIZE : ${maxSize}")
    topCounts.foreach(topCount => {

      println(s"CURRENT PROGRESS : ${percentage / maxSize}")

      maxThresholds.foreach { threshold => {

        println(s"CURRENT PROGRESS : ${percentage / maxSize}")


        println(s"Parameters:\n   TopCount:${topCount}\n   Threshold:${threshold}")

        new TagExperiment()
          .initParams(name, topCount, 10, 10, threshold)
          .evaluateNecessary(realEvalFolder, mainFolder)
          .summary(realEvalFolder)

        percentage += 1
        println(s"CURRENT PROGRESS : ${percentage / maxSize}")
      }
      }
    }
    )
  }

}
