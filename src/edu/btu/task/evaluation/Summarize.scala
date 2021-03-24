package edu.btu.task.evaluation

import java.io.File

import edu.btu.task.evaluation.Main.EvalKey

object Summarize {

  val folder = "resources/evaluations/"
  val folders = Array("single-approx", "multi-approx", "single-exact", "multi-exact","single-ngram","multi-ngram").map(folder + _) :+ folder

  def print(evalKey: EvalKey): Unit = {
    val eval = evalKey.eval
    println(s"Generation ID: ${eval.experimentParams.generationID()}")
    println(s"Method: ${eval.experimentParams.experimentCycle.mkString("--")}")
    println(s"Match select ratio: ${evalKey.ratio} Top count: ${evalKey.count} and Combine size: ${evalKey.eval.experimentParams.maxCombineSize} and Max regex size: ${evalKey.eval.experimentParams.maxRegexSize}")
    println(s"Geometric Mean: ${eval.geometricMean} Precision: ${eval.precision} Recall: ${eval.recall}  Accuracy:${eval.accuracy} FalsePositiveRate:${eval.falsepr} FalseNegativeRate:${eval.falsenr}")
    println(s"Counts:\n${eval.ratioCount.map { case (name, value) => "--->" + name + ":" + value }.mkString("\n")}")
  }


  def read(folder: String): Seq[EvalKey] = {

    new File(folder).listFiles().filter(_.isFile)
      .map(f => EvaluationResult(null).load(f.getAbsolutePath))
      .map(eval => {
        (eval.experimentParams.topCount, eval.experimentParams.maxPaths, eval.experimentParams.matchSelectRatio, eval)
      }).sortWith { case ((tcount1, psize1, msize1, _), (tcount2, psize2, msize2, _)) => {
      (msize1 < msize2) || (msize1 == msize2 && tcount1 < tcount2) || (msize1 == msize2 && tcount1 == tcount2 && psize1 < psize2)
    }}.map { case (count, paths, ratio, eval) => EvalKey(count, paths, ratio, folder, eval) }
      .toSeq

  }

  def compareMax(folders: Array[String], isEfficient:Boolean = false): Unit = {
    val mainFolders = folders.filter(folderPath=> !(folderPath.contains(ExperimentParams.singleNGRAM) || folderPath.contains(ExperimentParams.multiNGRAM)))
    val maxList = mainFolders.map(folder => {
      val elems = read(folder).sortBy(key=> if(isEfficient) key.eval.generationTime() else key.eval.geometricMean)
      if (!elems.isEmpty && !isEfficient) Some(elems.last)
      else if(!elems.isEmpty && isEfficient) Some(elems.head)
      else None
    }).flatten

    maxList.sortBy(_.eval.geometricMean).foreach(maxEval => {
      println("Max configuration : ")
      print(maxEval)
    })

  }

  def efficiencyMax(folders: Array[String]): Unit = {
    val allList = folders.flatMap(folder => read(folder))
    val forbidden = Array(ExperimentParams.singleNGRAM, ExperimentParams.multiNGRAM).map(_.toLowerCase)
    val mlist = allList.filter(evalKey=> !forbidden.exists(item=> evalKey.foldername.contains(item)))
    val maxList = mlist.groupBy(evalKey => evalKey.foldername).map(pair => {
      val elems = pair._2.sortBy(pp => pp.eval.geometricMean)
      if (!elems.isEmpty) Some(elems.last)
      else None
    }).toArray
      .flatten

    val efficientKey = maxList.sortBy(_.eval.generationTime()).head
    val finalKeys = searchParams(allList, efficientKey)
    finalKeys.sortBy(_.eval.geometricMean).foreach(maxEval => {
      println("Max configuration: ")
      print(maxEval)
    })
  }

  def accuracyMax(folders: Array[String]): Unit = {
    val allList = folders.flatMap(folder => read(folder))
    val forbidden = Array(ExperimentParams.singleNGRAM, ExperimentParams.multiNGRAM).map(_.toLowerCase)
    val mlist = allList.filter(evalKey=> !forbidden.exists(item=> evalKey.foldername.contains(item)))
    val maxList = mlist.groupBy(evalKey => evalKey.foldername).map(pair => {
      val elems = pair._2.sortBy(pp => pp.eval.geometricMean)
      if (!elems.isEmpty) Some(elems.last)
      else None
    }).toArray
      .flatten

    val efficientKey = maxList.sortBy(_.eval.geometricMean).reverse.head
    val finalKeys = searchParams(allList, efficientKey)
    finalKeys.foreach(maxEval => {
      println("Configuration: ")
      print(maxEval)
    })

  }

  def accuracyMaxComparison(folders: Array[String], topCount: Int = 5): Unit = {

    val allList = folders.flatMap(folder => read(folder))
    allList.groupBy(evalKey => evalKey.foldername).map(pair => {
      val elems = pair._2.sortBy(pp => pp.eval.geometricMean).reverse
      (pair._1, elems.take(topCount))
    }).foreach { case (folder, list) => {
      println("=============================================")
      println("Folder: " + folder)
      list.foreach(eval => print(eval))
    }}

  }

  def searchParams(allEvals: Array[EvalKey], targetKey: EvalKey): Seq[EvalKey] = {

    val finalResults = allEvals.groupBy(_.foldername)
      .toArray.map { case (foldername, array) => {
      val fn = array.find(key => key.equals(targetKey))
      fn
    }}.flatten

    finalResults
  }

  def summarize(folder: String): Unit = {

    val list = read(folder)

    println(s"Evaluation Folder ${folder}")
    list.foreach(evalKey => {
      print(evalKey)
    })

    val mainSort = list.sortBy(_.eval.geometricMean)
    println("Best geometric mean\n")
    mainSort.foreach(evalKey => {
      print(evalKey)
    })
  }

  def main(args: Array[String]): Unit = {
    compareMax(folders, true)
  }
}
