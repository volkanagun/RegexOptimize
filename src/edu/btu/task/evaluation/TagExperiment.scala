package edu.btu.task.evaluation

import java.io.{File, FileOutputStream, PrintWriter}

import edu.btu.operands.RegexGenerator
import edu.btu.task.tagmatch.{TagParser, TagSample}

import scala.io.Source

case class TrainTest(var train: Seq[TagSample], var test: Seq[TagSample]) {

  var trainSamplesByFilename = Map[String, Seq[TagSample]]()
  var trainSamplesByDomain = Map[String, Seq[TagSample]]()

  var testSamplesByFilename = Map[String, Seq[TagSample]]()
  var testSamplesByDomain = Map[String, Seq[TagSample]]()

  var trainingByFilename = Map[String, Seq[TagSample]]()
  var trainingByDomain = Map[String, Seq[TagSample]]()

  var testingByFilename = Map[String, Seq[TagSample]]()
  var testingByDomain = Map[String, Seq[TagSample]]()

  var trainAttrCount = 0.0
  var testAttrCount = 0.0

  def build(): this.type = {

    trainAttrCount = trainingByDomain.flatMap(_._2).map(_.properties.groupBy(_._1).size).sum
    testAttrCount = testingByDomain.flatMap(_._2).map(_.properties.groupBy(_._1).size).sum

    this

  }
}

case class EvaluationResult() {

  var tpCount = Map[String, Double]()
  var fpCount = Map[String, Double]()
  var tnCount = Map[String, Double]()
  var fnCount = Map[String, Double]()

  var ratioCount = Map[String, Double]()



  var count = 0
  var foldResults = Seq[EvaluationResult]()
  var groupResults = Map[String, EvaluationResult]()

  def groupSummary(): this.type = {
    println("Group Summary")

    groupResults.foreach { case (name, foldresult) => {
      println(s"Name:${name}")
      foldresult.subsummary()
    }
    }

    this
  }

  def subsummary(): this.type = {

    val precision = computePrecision()
    val recall = computeRecall()
    val fmeasure = computeFMeasure(precision, recall)
    val accuracy = computeAccuracy()

    println("Instance count: " + count)
    print("Precision", precision)
    print("Recall", recall)
    print("F-Measure", fmeasure)
    print("Accuracy", accuracy)
    this
  }

  def summary(): this.type = {

    val foldCount = foldResults.size
    System.out.println(s"Total number of experiments: ${count}")
    if (foldCount > 0) System.out.println(s"Total number of folds: ${foldCount}")




    foldResults.zipWithIndex.foreach {
      case (foldResult, i) => {
        System.out.println(s"Evaluation Fold: ${i}")
        foldResult.trainingSummary()
        foldResult.summary()
      }
    }

    println("===========++++++++++++++===========")
    println("===========Final Averages===========")
    println("===========++++++++++++++===========")
    println("===========++++++++++++++===========")
    trainingSummary()
    System.out.println(s"Total averages")

    print("True-Positive Count:", tpCount)
    print("False-Positive Count", fpCount)
    print("True-Negative Count", tnCount)
    print("False-Negative Count", fnCount)

    val precision = computePrecision()
    val recall = computeRecall()
    val fmeasure = computeFMeasure(precision, recall)
    val accuracy = computeAccuracy()

    print("Precision", precision)
    print("Recall", recall)
    print("F-Measure", fmeasure)
    print("Accuracy", accuracy)

    this
  }

  def trainingSummary(): this.type = {
    Seq("positive", "negative").foreach { name => {
      val ratioName = name + "-ratio"
      val sizeName = name + "-size"
      println(s"Training ratio for ${name} : ${ratioCount.getOrElse(ratioName, 0.0)}")
      println(s"Training size for ${name} : ${ratioCount.getOrElse(sizeName, 0.0)}")
    }
    }

    println(s"${ExperimentParams.sampleSize} : ${ratioCount.getOrElse(ExperimentParams.sampleSize, 0.0)}")
    println(s"${ExperimentParams.sampleRatio} : ${ratioCount.getOrElse(ExperimentParams.sampleRatio, 0.0)}")
    println(s"${ExperimentParams.fileSize} : ${ratioCount.getOrElse(ExperimentParams.fileSize, 0.0)}")
    println(s"${ExperimentParams.fileDomainRatio} : ${ratioCount.getOrElse(ExperimentParams.fileDomainRatio, 0.0)}")
    println(s"${ExperimentParams.fileSampleRatio} : ${ratioCount.getOrElse(ExperimentParams.fileSampleRatio, 0.0)}")

    this
  }

  def print(name: String, map: Map[String, Double]): this.type = {
    map.foreach { case (key, ccc) => System.out.println(s"${name}:${key}:${ccc}") }
    this
  }


  def computePrecision(): Map[String, Double] = {
    //positive prediction rate
    //precision
    tpCount.map { case (name, value) => {
      val sum = fpCount(name) + value
      (name, (value + 1E-10) / (sum + 1E-10))
    }
    }
  }

  def computeRecall(): Map[String, Double] = {
    //positive prediction rate
    //precision
    tpCount.map { case (name, value) => {
      val sum = fnCount(name) + value
      (name, (value + 1E-10) / (sum + 1E-10))
    }
    }
  }

  def computeFMeasure(precision: Map[String, Double], recall: Map[String, Double]): Map[String, Double] = {
    precision.map { case (name, prec) => {
      val rec = recall(name)
      (name, 2 * (prec * rec) / (prec + rec))
    }
    }
  }

  def computeAccuracy(): Map[String, Double] = {
    tpCount.map { case (name, tp) => {
      val fp = fpCount(name)
      val tn = tnCount(name)
      val fn = fnCount(name)
      val t = tn + tp + 1E-10
      (name, t / (tn + tp + fn + fp + 1E-10))
    }
    }
  }

  def append(name: String, evaluationResult: EvaluationResult): this.type = {
    groupResults = groupResults.updated(name, groupResults.getOrElse(name, EvaluationResult())
      .appendDirect(evaluationResult)
      .appendTraining(evaluationResult))
    this
  }

  def append(evaluationResult: EvaluationResult): this.type = {

    evaluationResult.tpCount.foreach { case (name, count) => incTP(name, count) }
    evaluationResult.fpCount.foreach { case (name, count) => incFP(name, count) }
    evaluationResult.tnCount.foreach { case (name, count) => incTN(name, count) }
    evaluationResult.fnCount.foreach { case (name, count) => incFN(name, count) }
    inc(evaluationResult.count)

    foldResults :+= evaluationResult
    this
  }

  def appendDirect(evaluationResult: EvaluationResult): this.type = {
    evaluationResult.tpCount.foreach { case (name, count) => incTP(name, count) }
    evaluationResult.fpCount.foreach { case (name, count) => incFP(name, count) }
    evaluationResult.tnCount.foreach { case (name, count) => incTN(name, count) }
    evaluationResult.fnCount.foreach { case (name, count) => incFN(name, count) }


    inc(evaluationResult.count)
    if (!evaluationResult.foldResults.isEmpty && foldResults.isEmpty) {
      foldResults ++= evaluationResult.foldResults
    }
    else if (!evaluationResult.foldResults.isEmpty) {
      foldResults.zip(evaluationResult.foldResults).foreach { case (mresult, nresult) => mresult.appendDirect(nresult) }
    }

    this
  }

  def save(): this.type = {
    //save the results to folder in readable format
    val fname = "resources/evaluations/crr" + ExperimentParams.generationID() + ".xml"
    val stream = new FileOutputStream(fname, false)
    val prw = new PrintWriter(stream)
    prw.println("<?xml version=\"1.0\" encoding=\"utf-8\"?>")
    prw.println("<ROOT>")
    var precisionTotal = 0.0
    var recallTotal = 0.0
    var fmeasureTotal = 0.0
    var accuracyTotal = 0.0
    var totalPositiveCount = 0.0
    var totalPositiveAttrRate = 0.0
    var totalNegativeCount = 0.0
    var totalNegativeAttrRate = 0.0
    var sampleCount = 0.0

    prw.println("<DOMAINS SIZE=\"" + groupResults.size + "\">")

    groupResults.foreach { case (domainName, result) => {

      val precision = result.computePrecision()
      val recall = result.computeRecall()
      val fmeasure = result.computeFMeasure(precision, recall)
      val accuracy = result.computeAccuracy()

      val precisionValue = precision.head._2
      val recallValue = recall.head._2
      val fmeasureValue = fmeasure.head._2
      val accuracyValue = accuracy.head._2

      precisionTotal += precisionValue
      recallTotal += recallValue
      fmeasureTotal += fmeasureValue
      accuracyTotal += accuracyValue

      sampleCount += result.count

      prw.println("<DOMAIN NAME=\"" + domainName + "\">")
      prw.println("<TRAINING>")
      Seq("positive", "negative").foreach { name => {
        val ratioName = name + "-ratio"
        val sizeName = name + "-size"
        val attrName = name + "-attr"

        if(name.equals("positive")){
          totalPositiveCount += result.ratioCount.getOrElse(sizeName, 0.0)
          totalPositiveAttrRate += result.ratioCount.getOrElse(attrName, 0.0)
        }
        else {
          totalNegativeCount += result.ratioCount.getOrElse(sizeName, 0.0)
          totalNegativeAttrRate += result.ratioCount.getOrElse(attrName, 0.0)
        }
        val ratioStr = "<RATIO  NAME=\"" + name + "\" VALUE=\"" + result.ratioCount.getOrElse(ratioName, 0.0) + "\"/>"
        val sizeStr = "<SIZE  NAME=\"" + name + "\" VALUE=\"" + result.ratioCount.getOrElse(sizeName, 0.0) + "\"/>"
        val attrStr = "<ATTR  NAME=\"" + name + "\" VALUE=\"" + result.ratioCount.getOrElse(attrName, 0.0) + "\"/>"

        prw.println(sizeStr)
        prw.println(attrStr)
        prw.println(ratioStr)

      }}

      prw.println("</TRAINING>")
      prw.println(s"<PRECISION>\n${precisionValue}\n</PRECISION>")
      prw.println(s"<RECALL>\n${recallValue}\n</RECALL>")
      prw.println(s"<FMEASURE>\n${fmeasureValue}\n</FMEASURE>")
      prw.println(s"<ACCURACY>\n${accuracyValue}\n</ACCURACY>")
      prw.println("</DOMAIN>")

    }
    }

    prw.println("</DOMAINS>")
    prw.println("<SUMMARY>")

    prw.println(s"<PRECISION>\n${precisionTotal / groupResults.size}\n</PRECISION>")
    prw.println(s"<RECALL>\n${recallTotal / groupResults.size}\n</RECALL>")
    prw.println(s"<FMEASURE>\n${fmeasureTotal / groupResults.size}\n</FMEASURE>")
    prw.println(s"<ACCURACY>\n${accuracyTotal / groupResults.size}\n</ACCURACY>")

    prw.println(s"<TRAINPOS>\n${totalPositiveCount}\n</TRAINPOS>")
    prw.println(s"<TRAINNEG>\n${totalNegativeCount}\n</TRAINNEG>")
    prw.println(s"<POSATTR>\n${totalPositiveAttrRate/groupResults.size}\n</POSATTR>")
    prw.println(s"<NEGATTR>\n${totalNegativeAttrRate/groupResults.size}\n</NEGATTR>")

    prw.println(s"<COUNT>\n${sampleCount}\n</COUNT>")
    prw.println("</SUMMARY>")

    prw.println(ExperimentParams.paramsXML())
    prw.println(TimeBox.xml())

    prw.println("</ROOT>")
    prw.close();
    this
  }

  /*def filterSave():this.type ={
    groupResults = groupResults.filter{case(name, eval)=> eval.computePrecision().head._2 > 0.3}
    save()
  }*/

  def appendTraining(trainingResult: EvaluationResult): this.type = {
    trainingResult.ratioCount.foreach { case (name, count) => incRatio(name, count) }
    this
  }

  def append(name: String, map: Map[String, Double], count: Double): Map[String, Double] = {
    map.updated(name, map.getOrElse(name, 0.0) + count)
  }

  def incRatio(name: String, defaultValue: Double = 1.0): this.type = {
    ratioCount = append(name, ratioCount, defaultValue)
    this
  }
  def incRatioBy(name: String, updateValue: Double): this.type = {
    ratioCount = ratioCount.updated(name, ratioCount.getOrElse(name, 0.0)+updateValue)
    this
  }

  def incTP(name: String, defaultValue: Double = 1.0): this.type = {
    tpCount = append(name, tpCount, defaultValue)
    this
  }

  def incFP(name: String, defaultValue: Double = 1.0): this.type = {
    fpCount = append(name, fpCount, defaultValue)
    this
  }

  def incTN(name: String, defaultValue: Double = 1.0): this.type = {
    tnCount = append(name, tnCount, defaultValue)
    this
  }

  def incFN(name: String, defaultValue: Double = 1.0): this.type = {
    fnCount = append(name, fnCount, defaultValue)
    this
  }

  def inc(defaultValue: Int = 1): this.type = {
    count += defaultValue
    this
  }

}


class TagExperiment {
  //measure efficiency of tag generation
  //measure efficiency of filtering
  //measure accuracy

  //cross-validation
  //training vs test vs all
  var trainTestRatio = Array(0.6, 0.4)


  var totalSampleCount = 0
  var totalDomainCount = 0
  var totalFileCount = 0

  var totalTrainingTime = 0L
  var totalTestingTime = 0L
  var evaluationResult = Map[String, EvaluationResult]()

  //number of samples per domain
  var allsamples = Seq[TagSample]()
  var domainSampleMap = Map[String, Seq[TagSample]]()
  var domainFileMap = Map[String, Seq[String]]()
  var fileSampleMap = Map[String, Seq[TagSample]]()
  var domainValidateMap = Map[String, TrainTest]()

  def initParams(): this.type = {
    ExperimentParams.loadXML().saveXML()
    this
  }

  def initParams(name:String, topCount:Int, maxCombineSize:Int): this.type = {
    val experimentParams = ExperimentParams.loadXML()
    experimentParams.experimentCycle = Array(name)
    experimentParams.topCount = topCount
    experimentParams.maxCombineSize = maxCombineSize
    experimentParams.saveXML()
    experimentParams.loadXML()
    this
  }


  //do the train/test for each domain
  //do the accuracy for each domain
  def readFile(filename: String): this.type = {

    allsamples ++= TagParser.read(filename)
    domainSampleMap ++= allsamples.groupBy(_.domain)
    fileSampleMap ++= allsamples.groupBy(_.filename)
    domainFileMap ++= domainSampleMap.mapValues(samples => samples.map(_.filename)
      .distinct)

    totalSampleCount = allsamples.size
    totalDomainCount = domainSampleMap.size
    totalFileCount = fileSampleMap.size
    this

  }

  //do the train/test for each domain
  //do the accuracy for each domain
  def readCSV(filename: String): this.type = {

    allsamples ++= TagParser.readCSV(filename)
    domainSampleMap ++= allsamples.groupBy(_.domain)
    fileSampleMap ++= allsamples.groupBy(_.filename)
    domainFileMap ++= domainSampleMap.mapValues(samples => samples.map(_.filename)
      .distinct)

    totalSampleCount = allsamples.size
    totalDomainCount = domainSampleMap.size
    totalFileCount = fileSampleMap.size

    this
  }

  //do the train/test for each domain
  //do the accuracy for each domain
  def readCSVFolder(folder: String): this.type = {
    new File(folder).list().filter(_.endsWith("csv")).foreach(filename => readCSV(folder + filename))
    this
  }


  //build from train and test cases
  //construct
  def buildSamples(trainTest: => TrainTest): TrainTest = {

    println(s"Building samples for train-test")
    val crrTrainFileMap = trainTest.train.groupBy(_.filename)
    val crrTrainDomainMap = trainTest.train.groupBy(_.domain)
    val crrTrainDomainFileMap = crrTrainDomainMap.mapValues(samples => samples.map(_.filename)
      .distinct)

    val crrTestFileMap = trainTest.test.groupBy(_.filename)
    val crrTestDomainMap = trainTest.test.groupBy(_.domain)
    val crrTestDomainFileMap = crrTestDomainMap.mapValues(samples => samples.map(_.filename)
      .distinct)

    //find the bug
    trainTest.trainSamplesByFilename = crrTrainFileMap.keys.map(filename => filename -> createSamplesByFilename(filename, crrTrainFileMap)).toMap
    trainTest.trainSamplesByDomain = crrTrainDomainFileMap.keys.map(domain => domain -> createSamplesByDomain(domain, crrTrainDomainFileMap, crrTrainFileMap)).toMap

    trainTest.testSamplesByFilename = crrTestFileMap.keys.map(filename => filename -> createSamplesByFilename(filename, crrTestFileMap)).toMap
    trainTest.testSamplesByDomain = crrTestDomainMap.keys.map(domain => domain -> createSamplesByDomain(domain, crrTestDomainFileMap, crrTestFileMap)).toMap

    trainTest.trainingByFilename = crrTrainFileMap.keys.map(filename => filename -> trainingSamplesByFilename(filename, crrTrainFileMap)).toMap
    trainTest.trainingByDomain = crrTrainDomainFileMap.keys.map(domain => domain -> trainingSamplesByDomain(domain, crrTrainDomainFileMap, crrTrainFileMap)).toMap

    trainTest.testingByFilename = crrTestFileMap.keys.map(filename => filename -> trainingSamplesByFilename(filename, crrTestFileMap)).toMap
    trainTest.testingByDomain = crrTestDomainMap.keys.map(domain => domain -> trainingSamplesByDomain(domain, crrTestDomainFileMap, crrTestFileMap)).toMap

    println("Building finished...")
    trainTest
  }

  def trainingSamplesByFilename(filename: String, crrFileSample: Map[String, Seq[TagSample]]): Seq[TagSample] = {

    if (new File(filename).exists()) {

      val htmlText = Source.fromFile(filename, "UTF-8").getLines().mkString("\n")
      val domain = crrFileSample(filename).map(_.domain).distinct.head
      val allRegexTags = crrFileSample(filename).map(_.createRegex()).distinct
      val allSamples = allRegexTags.flatMap(crrRegexTag => crrRegexTag.r.findAllIn(htmlText).map(matched => TagSample(matched, filename, domain)))
      val positiveSamples = crrFileSample(filename)
      val negativeSamples = allSamples.filter(tagSample => !positiveSamples.contains(tagSample))
        .map(_.setNegative())
      positiveSamples ++ negativeSamples
    }
    else {

      val allSamples = crrFileSample(filename)
      val positiveSamples = allSamples.filter(tagSample => !tagSample.isNegative)
      val negativeSamples = allSamples.filter(tagSample => tagSample.isNegative)
      positiveSamples ++ negativeSamples

    }

  }

  def createSamplesByFilename(filename: String, crrFileSample: Map[String, Seq[TagSample]]): Seq[TagSample] = {
    if (new File(filename).exists()) {

      val htmlText = Source.fromFile(filename, "UTF-8").getLines().mkString("\n")
      val domain = crrFileSample(filename).head.domain
      val allRegexTags = crrFileSample(filename).map(_.createRegex()).distinct
      val allSamples = allRegexTags.flatMap(crrRegexTag => crrRegexTag.r.findAllIn(htmlText).map(matched => TagSample(matched, filename, domain)))
      val positiveSamples = crrFileSample(filename)
      val negativeSamples = allSamples.filter(tagSample => !positiveSamples.contains(tagSample))
        .map(_.setNegative())
      val positiveTagGroup = positiveSamples.groupBy(_.tagName)
      val negativeTagGroup = negativeSamples.groupBy(_.tagName)
      createSamples(positiveTagGroup, negativeTagGroup, filename, domain)

    }
    else {

      val allSamples = crrFileSample(filename)
      val domain = allSamples.head.domain
      val positiveSamples = crrFileSample(filename).filter(tagSample => !tagSample.isNegative)
      val negativeSamples = allSamples.filter(tagSample => tagSample.isNegative)
      val positiveTagGroup = positiveSamples.groupBy(_.tagName)
      val negativeTagGroup = negativeSamples.groupBy(_.tagName)
      createSamples(positiveTagGroup, negativeTagGroup, filename, domain)

    }

  }


  def trainingSamplesByDomain(domain: String, crrDomainFileMap: Map[String, Seq[String]], crrFileMap: Map[String, Seq[TagSample]]): Seq[TagSample] = {
    //for each filename in this domain create samples and merge them to single
    var positiveAllSamples = Seq[TagSample]()
    var negativeAllSamples = Seq[TagSample]()

    crrDomainFileMap(domain).foreach(filename => {

      if (new File(filename).exists()) {

        val htmlText = Source.fromFile(filename, "UTF-8").getLines().mkString("\n")
        val allRegexTags = crrFileMap(filename).map(_.createRegex()).distinct
        val allSamples = allRegexTags.flatMap(crrRegexTag => crrRegexTag.r.findAllIn(htmlText).map(matched => TagSample(matched, filename, domain)))
        val positiveSamples = crrFileMap(filename)
        val negativeSamples = allSamples.filter(tagSample => !positiveSamples.contains(tagSample)).map(_.setNegative())
        positiveAllSamples ++= positiveSamples
        negativeAllSamples ++= negativeSamples

      }
      else {

        val allSamples = crrFileMap(filename)
        val positiveSamples = allSamples.filter(tagSample => !tagSample.isNegative)
        val negativeSamples = allSamples.filter(tagSample => tagSample.isNegative)
        positiveAllSamples ++= positiveSamples
        negativeAllSamples ++= negativeSamples
      }

    })

    positiveAllSamples ++ negativeAllSamples

  }

  def createSamplesByDomain(domain: String, crrDomainFileMap: Map[String, Seq[String]], crrFileMap: Map[String, Seq[TagSample]]): Seq[TagSample] = {
    //for each filename in this domain create samples and merge them to single
    var positiveAllSamples = Seq[TagSample]()
    var negativeAllSamples = Seq[TagSample]()

    crrDomainFileMap(domain).foreach(filename => {
      if (new File(filename).exists()) {

        val htmlText = Source.fromFile(filename, "UTF-8").getLines().mkString("\n")
        val allRegexTags = crrFileMap(filename).map(_.createRegex()).distinct
        val allSamples = allRegexTags.flatMap(crrRegexTag => crrRegexTag.r.findAllIn(htmlText).map(matched => TagSample(matched, filename, domain)))
        val positiveSamples = crrFileMap(filename)
        val negativeSamples = allSamples.filter(tagSample => !positiveSamples.contains(tagSample)).map(_.setNegative())
        positiveAllSamples ++= positiveSamples
        negativeAllSamples ++= negativeSamples

      }
      else {

        val allSamples = crrFileMap(filename)
        val positiveSamples = allSamples.filter(tagSample => !tagSample.isNegative)
        val negativeSamples = allSamples.filter(tagSample => tagSample.isNegative)
        positiveAllSamples ++= positiveSamples
        negativeAllSamples ++= negativeSamples

      }
    })

    val positiveTagGroup = positiveAllSamples.groupBy(_.tagName)
    val negativeTagGroup = negativeAllSamples.groupBy(_.tagName)

    createSamples(positiveTagGroup, negativeTagGroup, domain)

  }

  def createSamples(positiveTagGroup: Map[String, Seq[TagSample]], negativeTagGroup: Map[String, Seq[TagSample]], filename: String, domain: String): Seq[TagSample] = {

    val newSamples = positiveTagGroup.map { case (tagName, positives) => {
      val newTagSample = TagSample(tagName).setFilename(filename).setDomain(domain)
      if (negativeTagGroup.contains(tagName)) {
        newTagSample.unisectBySamples(positives)
          .differenceBySamples(negativeTagGroup(tagName))
      }
      else {
        newTagSample.unisectBySamples(positives)
      }
    }
    }

    newSamples.toSeq
  }

  def createSamples(positiveTagGroup: Map[String, Seq[TagSample]], negativeTagGroup: Map[String, Seq[TagSample]], domain: String): Seq[TagSample] = {

    val newSamples = positiveTagGroup.map { case (tagName, positives) => {
      val newTagSample = TagSample(tagName)
        .setFilename(domain)
        .setDomain(domain)

      if (negativeTagGroup.contains(tagName)) {
        newTagSample.unisectBySamples(positives)
          .differenceBySamples(negativeTagGroup(tagName))
      }
      else {

        newTagSample.unisectBySamples(positives)

      }

      newTagSample.filter()

    }
    }

    newSamples.toSeq
  }

  //accuracy evaluation
  def evaluateMatch(name: String, decideAverage: Double, positives: Map[String, Set[String]], testingSet: Set[TagSample]): EvaluationResult = {
    val foundMatches = testingSet.par.filter(tagSample => tagSample.matchWithPositive(positives) > decideAverage)

    //true positives
    val tp = foundMatches.filter(!_.isNegative)
    val fp = foundMatches.filter(_.isNegative)
    val tn = testingSet.filter(_.isNegative).filter(!foundMatches.contains(_))
    val fn = testingSet.filter(!_.isNegative).filter(!foundMatches.contains(_))

    EvaluationResult()
      .inc(testingSet.size)
      .incTP(name, tp.size)
      .incFP(name, fp.size)
      .incTN(name, tn.size)
      .incFN(name, fn.size)

  }

  //accuracy evaluation
  //override negatives rather than positives because there are more samples

  def evaluateMatch(name: String, decideAverage: (Double, Double), positives: Map[String, Set[String]], negatives: Map[String, Set[String]], testingSet: Set[TagSample]): EvaluationResult = {

    val avgNeg = decideAverage._2 + decideAverage._2 / 5
    val avgPos = decideAverage._1

    val foundAverages = testingSet.map(sp => (sp, sp.matchWithNegative(positives, negatives)))
    val foundPositiveMatches = foundAverages.filter { case (_, (pos, neg)) => pos >= avgPos || (pos > neg && neg < avgNeg) }.map(_._1)
    val foundNegativeMatches = foundAverages.filter { case (_, (pos, neg)) => neg >= avgNeg || neg >= pos }.map(_._1) -- foundPositiveMatches

    val positiveResults = foundPositiveMatches -- foundNegativeMatches
    val negativeResults = foundNegativeMatches -- foundPositiveMatches


    //true positives
    val tp = testingSet.filter(!_.isNegative).filter(positiveResults.contains(_))
    val fp = testingSet.filter(_.isNegative).filter(positiveResults.contains(_))

    val tn = testingSet.filter(_.isNegative).filter(ts => negativeResults.contains(ts))
    val fn = testingSet.filter(!_.isNegative).filter(ts => negativeResults.contains(ts))


    EvaluationResult()
      .inc(testingSet.size)
      .incTP(name, tp.size)
      .incFP(name, fp.size)
      .incTN(name, tn.size)
      .incFN(name, fn.size)

  }

  def evaluateMatchTimely(name: String, decideAverage: Double, positives: Map[String, Set[String]], testingSet: Set[TagSample]): EvaluationResult = {
    TimeBox.measureTime[EvaluationResult](name, testingSet.size.toLong, evaluateMatch(name, decideAverage, positives, testingSet))
  }

  def evaluateMatchTimely(name: String, decideAverage: (Double, Double), positives: Map[String, Set[String]], negatives: Map[String, Set[String]], testingSet: Set[TagSample]): EvaluationResult = {
    TimeBox.measureTime[EvaluationResult](name, testingSet.size.toLong, evaluateMatch(name, decideAverage, positives, negatives, testingSet))
  }

  def evaluate(domainName: String, mainResult: EvaluationResult, trainTest: => TrainTest, foldNum: Int): this.type = {
    println(s"Evaluating training dataset")

    evaluate(domainName, mainResult, trainTest.train.toSet, trainTest.test.toSet, foldNum)
    this
  }

  def evaluateTraining(mainResult: EvaluationResult, generators: Array[(String, Seq[RegexGenerator])]): this.type = {

    generators.foreach(_._2.foreach(rgen => mainResult.appendTraining(rgen.trainingEval)))

    this
  }

  def trainingAverage(trainingSet: Set[TagSample], map: Map[String, Set[String]]): Double = {
    val sz = trainingSet.size
    val positives = trainingSet.filter(!_.isNegative).toSeq.map(tg => tg.matchWithPositive(map))
    val negatives = trainingSet.filter(_.isNegative).toSeq.map(tg => tg.matchWithPositive(map))
    val positiveAvg = positives.sum / positives.length
    val negativeAvg = negatives.sum / negatives.length
    val difference = (positiveAvg - negativeAvg) / 8
    val result = positiveAvg - difference
    result
  }

  //create two averages from positives and negative examples


  def average(sequence: Seq[(Double, Double)]): (Double, Double) = {
    var p1 = 0.0;
    var p2 = 0.0;
    sequence.foreach { case (i1, i2) => p1 += i1 / sequence.size; p2 += i2 / sequence.size }
    (p1, p2)
  }

  def trainingAverage(positiveSet: Set[TagSample], negativeSet: Set[TagSample], mapPositive: Map[String, Set[String]], mapNegative: Map[String, Set[String]]): (Double, Double) = {
    val avgPos = average(positiveSet.toSeq.map(tg => tg.matchWithNegative(mapPositive, mapNegative)))
    val avgNeg = average(negativeSet.toSeq.map(tg => tg.matchWithNegative(mapNegative, mapPositive)))

    avgPos
  }

  def evaluate(domainName: String, mainResult: EvaluationResult, trainingSet: Set[TagSample], testingSet: Set[TagSample], foldNum: Int): this.type = {


    val regexGenMap0 = ExperimentParams.regexGenerator(trainingSet).toArray
    val regexGenMap1 = ExperimentParams.combineGenerator(regexGenMap0)
    val regexGenMap = ExperimentParams.loadGenerator(ExperimentParams.filterGenerator(regexGenMap1), ExperimentParams.generationMapID(domainName, foldNum))

    //val regexTest = regexGenMap.head._2.head.generate()

    val trainingMap = regexGenMap.map { case (name, regexGenerators) => {

      val regexStrings = regexGenerators.map(gen=> gen.generateTimely())
        .filter(!_.isEmpty)
      (name -> regexStrings)

    }}.filter { case (name, set) => !set.isEmpty }.toMap

    val eval = if (ExperimentParams.isSingle()) {
      val name = "evaluation-single-regex"
      val positiveMap = trainingMap.mapValues(_.head)
      val trainDecide = trainingAverage(trainingSet, positiveMap)
      evaluateTraining(mainResult, regexGenMap)
      evaluateMatchTimely(name, trainDecide, positiveMap, testingSet)
    }
    else {
      val name = "evaluation-multi-regex"
      val positiveMap = trainingMap.mapValues(_.head)
      val negativeMap = trainingMap.mapValues(_.last)
      val positiveSamples = trainingSet.filter(!_.isNegative)
      val negativeSamples = trainingSet.filter(_.isNegative)
      val trainDecide = trainingAverage(positiveSamples, negativeSamples, positiveMap, negativeMap)
      evaluateTraining(mainResult, regexGenMap)
      evaluateMatchTimely(name, trainDecide, positiveMap, negativeMap, testingSet)
    }


    mainResult.append(eval)

    this
  }

  def summary(): EvaluationResult = {
    val finalResult = EvaluationResult()
    evaluationResult.foreach { case (domainName, evalResult) => {
      finalResult.appendDirect(evalResult)
      finalResult.appendTraining(evalResult)
      finalResult.append(domainName, evalResult)
    }
    }

    finalResult.groupSummary()
    finalResult.summary()
    finalResult.save()
  }

  //evaluate each domain separetely
  def evaluate(folder: String): this.type = {

    lazy val mainsamples = ExperimentParams.loadSamples(readCSVFolder(folder).allsamples)

    val domainsamples = if (ExperimentParams.selectedDomains.isEmpty) mainsamples.groupBy(_.domain)
    else mainsamples.groupBy(_.domain).filter { case (domainName, _) => ExperimentParams.selectedDomains.exists(selectedName => domainName.contains(selectedName)) }

    val filtersamples = domainsamples.filter { case (name, seq) => {
      val pp = seq.filter(_.isNegative)
      val ss = seq.filter(!_.isNegative)
      pp.length >= ExperimentParams.minimumPositiveSamples && ss.length >= ExperimentParams.k * 2
    }}

    println(s"Number of domains: ${filtersamples.size}")

    evaluationResult = filtersamples.par.map {

      case (domainName, ccsamples) => {

        val trainTestSeq = crossvalidate(ExperimentParams.k, ccsamples, ExperimentParams.maxSamples)

        val mainEval = EvaluationResult()

        val fileSize = domainFileMap.getOrElse(domainName, Seq()).length + 1
        mainEval.incRatioBy(ExperimentParams.sampleSize,  ccsamples.length)
        mainEval.incRatioBy(ExperimentParams.sampleRatio, ccsamples.length.toDouble / filtersamples.size)
        mainEval.incRatioBy(ExperimentParams.fileSize, fileSize)
        mainEval.incRatioBy(ExperimentParams.fileDomainRatio, fileSize / filtersamples.size)
        mainEval.incRatioBy(ExperimentParams.fileSampleRatio, (ccsamples.length / fileSize) / filtersamples.size)


        trainTestSeq.zipWithIndex.foreach { case (trainTest, foldNum) => {
          lazy val ntt = buildSamples(trainTest)
          evaluate(domainName, mainEval, ntt, foldNum)
        }}

        (domainName -> mainEval)
      }
    }.toArray.toMap

    this

  }

  def crossvalidate(k: Int, thisSamples: Seq[TagSample], maxSize: Int = 5): Seq[TrainTest] = {
    var main = Seq[TrainTest]()

    thisSamples.groupBy(_.domain).foreach {
      case (domain, samples) => {
        println(s"Splitting dataset for cross-validation of ${k}")
        val positives = samples.filter(!_.isNegative).take(maxSize)
        val negatives = samples.filter(_.isNegative).take(maxSize)
        val posDist = positives.length.toDouble / k
        val negDist = negatives.length.toDouble / k

        if (posDist >= 2) {
          main = crossUpdate(main, crossvalidateAll(k, positives))
          main = crossUpdate(main, crossvalidateAll(k, negatives))
        }
      }
    }

    main.map(_.build())
  }

  def crossUpdate(main: Seq[TrainTest], crr: Seq[TrainTest]): Seq[TrainTest] = {
    if (main.length == crr.length) {
      main.zip(crr).foreach {
        case (m, c) => m.train = m.train ++ c.train;
          m.test ++= c.test
      }
      main
    }
    else {
      crr
    }
  }

  def crossvalidateAll(k: Int, crrDomainSamples: Seq[TagSample]): Seq[TrainTest] = {


    //consider positives and negatives
    var crrSamples = crrDomainSamples
    val splitSize = crrDomainSamples.size / k
    var splitSeqs = Seq[Seq[TagSample]]()

    for (i <- 0 until k - 1) {
      var seq = crrSamples.take(splitSize)
      crrSamples = crrSamples.takeRight(crrSamples.size - splitSize)
      splitSeqs = splitSeqs :+ crrSamples
    }

    splitSeqs = splitSeqs :+ crrSamples
    val rangeFull = Range(0, splitSeqs.size).toSet[Int]
    var trainTestSeq = Seq[TrainTest]()

    for (i <- 0 until splitSeqs.size) {

      val newRange = rangeFull - i
      val trainSeq = splitSeqs(i)
      val testSeq = newRange.flatMap(splitSeqs(_)).toSeq
      trainTestSeq :+= TrainTest(trainSeq, testSeq)

    }

    trainTestSeq
  }


}
