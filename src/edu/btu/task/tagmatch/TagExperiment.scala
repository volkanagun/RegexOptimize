package edu.btu.task.tagmatch

import java.io.File

import edu.btu.operands.{RegexGenerator, RegexString}
import edu.btu.task.tagmatch.TagExperimentCodes.experimentCycle
import edu.btu.task.tagmatch.TimeBox.{countMap, timeMap}

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
}

case class EvaluationResult() {

  var tpCount = Map[String, Int]()
  var fpCount = Map[String, Int]()
  var tnCount = Map[String, Int]()
  var fnCount = Map[String, Int]()
  var count = 0
  var foldResults = Seq[EvaluationResult]()

  def summary(): this.type = {
    System.out.println(s"Total number of experiments: ${count}")
    System.out.println(s"Total number of folds: ${foldResults.length}")
    System.out.println(s"Total averages")

    print("True-Positives", tpCount)
    print("False-Positives", fpCount)
    print("True-Negatives", tnCount)
    print("False-Negatives", fnCount)

    foldResults.zipWithIndex.foreach {
      case (foldResult, i) => {
        System.out.println(s"Evaluation Fold: ${i}")
        foldResult.summary()
      }
    }

    this
  }

  def print(name: String, map: Map[String, Int]): this.type = {
    map.foreach { case (key, ccc) => System.out.println(s"${name}:${key}:${ccc.toDouble / count}") }
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

  def append(name: String, map: Map[String, Int], count: Int): Map[String, Int] = {
    map.updated(name, map.getOrElse(name, 0) + count)
  }

  def incTP(name: String, defaultValue: Int = 1): this.type = {
    tpCount = append(name, tpCount, defaultValue)
    this
  }

  def incFP(name: String, defaultValue: Int = 1): this.type = {
    fpCount = append(name, fpCount, defaultValue)
    this
  }

  def incTN(name: String, defaultValue: Int = 1): this.type = {
    tnCount = append(name, tnCount, defaultValue)
    this
  }

  def incFN(name: String, defaultValue: Int = 1): this.type = {
    fnCount = append(name, fnCount, defaultValue)
    this
  }

  def inc(defaultValue: Int = 1): this.type = {
    count += defaultValue
    this
  }

}

object TagExperimentCodes {

  val singleExact = "SINGLE-EXACT"
  val singleApprox = "SINGLE-APPROX"
  val multiExact = "MULTI-EXACT"
  val multiApprox = "MULTI-APPROX"

  //search with multiple regexes
  val regexMulti = "REGEX-MULTI"
  //search with single regex construct longer regex
  val regexSingle = "REGEX-SINGLE"

  //use filenames or domains
  val samplesFromDomain = "DOMAIN-SAMPLES"
  val samplesFromFilename = "FILENAME-SAMPLES"

  var k = 3
  var folder = "resources/img-csvs/"
  var datasets = "resources/datasets/"

  var experimentCycle = Array[String](singleExact, regexMulti)

  def regexGenerator(training: Set[TagSample]): Map[String, Seq[RegexGenerator]] = {

    if (experimentCycle.contains(singleExact) && experimentCycle.contains(regexSingle)) {
      val positiveMap = training.flatMap(tg => tg.positiveRegex.multimap.flatMap { case (tag, set) => set.map(item => tag -> item) })
        .groupBy(_._1).mapValues(_.map(_._2)).mapValues(positiveCases => Seq(RegexString.applyExact(positiveCases)))
      positiveMap
    }
    else if (experimentCycle.contains(singleApprox) && experimentCycle.contains(regexSingle)) {
      val positiveMap = training.flatMap(tg => tg.positiveRegex.multimap.flatMap { case (tag, set) => set.map(item => tag -> item) })
        .groupBy(_._1).mapValues(_.map(_._2)).mapValues(positiveCases => Seq(RegexString.applyApproximate(positiveCases)))
      positiveMap
    }
    else if (experimentCycle.contains(singleExact) && experimentCycle.contains(regexMulti)) {

      val positiveMap = training.flatMap(tg => tg.positiveRegex.multimap.flatMap { case (tag, set) => set.map(item => tag -> item) })
        .groupBy(_._1).mapValues(_.map(_._2)).mapValues(positiveCases => Seq(RegexString.applyExact(positiveCases)))

      val negativeMap = training.flatMap(tg => tg.negativeRegex.multimap.flatMap { case (tag, set) => set.map(item => tag -> item) })
        .groupBy(_._1).mapValues(_.map(_._2)).mapValues(positiveCases => Seq(RegexString.applyExact(positiveCases)))

      positiveMap.map { case (tag, pos) => tag -> (pos ++ negativeMap.getOrElse(tag, Seq())) }
    }
    else if (experimentCycle.contains(singleApprox) && experimentCycle.contains(regexMulti)) {

      val positiveMap = training.flatMap(tg => tg.positiveRegex.multimap.flatMap { case (tag, set) => set.map(item => tag -> item) })
        .groupBy(_._1).mapValues(_.map(_._2)).mapValues(positiveCases => Seq(RegexString.applyApproximate(positiveCases)))

      val negativeMap = training.flatMap(tg => tg.negativeRegex.multimap.flatMap { case (tag, set) => set.map(item => tag -> item) })
        .groupBy(_._1).mapValues(_.map(_._2)).mapValues(positiveCases => Seq(RegexString.applyApproximate(positiveCases)))

      positiveMap.map { case (tag, pos) => tag -> (pos ++ negativeMap.getOrElse(tag, Seq())) }
    }
    else if (experimentCycle.contains(multiExact) && experimentCycle.contains(regexSingle)) {

      val positiveMap = training.flatMap(tg => tg.positiveRegex.multimap.flatMap { case (tag, set) => set.map(item => tag -> item) })
        .groupBy(_._1).mapValues(_.map(_._2))
      val negativeMap = training.flatMap(tg => tg.negativeRegex.multimap.flatMap { case (tag, set) => set.map(item => tag -> item) })
        .groupBy(_._1).mapValues(_.map(_._2))

      positiveMap.map { case (tag, positiveCases) => (tag, positiveCases, negativeMap.getOrElse(tag, Set())) }
        .map { case (tag, pos, neg) => tag -> Seq(RegexString.applyExact(pos, neg)) }.toMap

    }
    else if (experimentCycle.contains(multiApprox) && experimentCycle.contains(regexSingle)) {

      val positiveMap = training.flatMap(tg => tg.positiveRegex.multimap.flatMap { case (tag, set) => set.map(item => tag -> item) })
        .groupBy(_._1).mapValues(_.map(_._2))
      val negativeMap = training.flatMap(tg => tg.negativeRegex.multimap.flatMap { case (tag, set) => set.map(item => tag -> item) })
        .groupBy(_._1).mapValues(_.map(_._2))

      positiveMap.map { case (tag, positiveCases) => (tag, positiveCases, negativeMap.getOrElse(tag, Set())) }
        .map { case (tag, pos, neg) => tag -> Seq(RegexString.applyApproximate(pos, neg)) }.toMap

    }
    else Map()


  }

  def isSingle(): Boolean = {
    experimentCycle.contains(singleApprox) || experimentCycle.contains(singleExact) || experimentCycle.contains(regexSingle)
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
  var evaluationResult = EvaluationResult()


  //number of samples per domain
  var allsamples = Seq[TagSample]()
  var domainSampleMap = Map[String, Seq[TagSample]]()
  var domainFileMap = Map[String, Seq[String]]()
  var fileSampleMap = Map[String, Seq[TagSample]]()
  var domainValidateMap = Map[String, TrainTest]()


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
    new File(folder).list().foreach(filename => readCSV(folder+filename))
    this
  }


  //build from train and test cases
  //construct
  def buildSamples(trainTest: TrainTest): this.type = {

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

    this
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

    }}

    newSamples.toSeq
  }

  //accuracy evaluation
  def evaluateMatch(name: String, positives: Map[String, Set[String]], testingSet: Set[TagSample]): EvaluationResult = {
    val foundMatches = testingSet.par.filter(tagSample => tagSample.matchWithPositive(positives))

    //true positives
    val tp = foundMatches.filter(!_.isNegative)
    val fp = foundMatches.filter(_.isNegative)
    val tn = testingSet.filter(!_.isNegative).filter(!foundMatches.contains(_))
    val fn = testingSet.filter(_.isNegative).filter(!foundMatches.contains(_))

    EvaluationResult()
      .inc(testingSet.size)
      .incTP(name, tp.size)
      .incFP(name, fp.size)
      .incTN(name, tn.size)
      .incFN(name, fn.size)

  }

  //accuracy evaluation
  def evaluateMatch(name: String, positives: Map[String, Set[String]], negatives: Map[String, Set[String]], testingSet: Set[TagSample]): EvaluationResult = {

    val foundMatches = testingSet.filter(tagSample => tagSample.matchWithNegative(positives, negatives))

    //true positives
    val tp = foundMatches.filter(!_.isNegative)
    val fp = foundMatches.filter(_.isNegative)
    val tn = testingSet.filter(!_.isNegative).filter(!foundMatches.contains(_))
    val fn = testingSet.filter(_.isNegative).filter(!foundMatches.contains(_))

    EvaluationResult()
      .inc(testingSet.size)
      .incTP(name, tp.size)
      .incFP(name, fp.size)
      .incTN(name, tn.size)
      .incFN(name, fn.size)

  }

  def evaluateMatchTimely(name: String, positives: Map[String, Set[String]], testingSet: Set[TagSample]): EvaluationResult = {
    TimeBox.measureTime[EvaluationResult](name, evaluateMatch(name, positives, testingSet))
  }

  def evaluateMatchTimely(name: String, positives: Map[String, Set[String]], negatives: Map[String, Set[String]], testingSet: Set[TagSample]): EvaluationResult = {
    TimeBox.measureTime[EvaluationResult](name, evaluateMatch(name, positives, negatives, testingSet))
  }

  def evaluate(trainTest: TrainTest): this.type = {
    evaluate(trainTest.train.toSet, trainTest.test.toSet)
    this
  }

  def evaluate(trainingSet: Set[TagSample], testingSet: Set[TagSample]): this.type = {
    val regexGenMap = TagExperimentCodes.regexGenerator(trainingSet).toArray
    val trainingMap = regexGenMap.map {case(name, regexGenerators)=>{(name ->
      regexGenerators.map(_.generate()))}}
      .toMap

    val eval = if (TagExperimentCodes.isSingle()) {
      val name = "evaluation-single-regex"
      val positiveMap = trainingMap.mapValues(_.head)
      evaluateMatchTimely(name, positiveMap, testingSet)
    }
    else {
      val name = "evaluation-multi-regex"
      val positiveMap = trainingMap.mapValues(_.head)
      val negativeMap = trainingMap.mapValues(_.last)
      evaluateMatchTimely(name, positiveMap, negativeMap, testingSet)
    }

    evaluationResult.append(eval)
    this
  }

  def evaluate(folder:String): EvaluationResult = {
    val allsamples = readCSVFolder(folder).allsamples
    //now domain and positives

    val trainTestSeq = crossvalidate(TagExperimentCodes.k, allsamples)

    trainTestSeq.par.foreach { trainTest  => {
      buildSamples(trainTest)
    }}

    trainTestSeq.foreach(trainTest => {
      evaluate(trainTest)
    })

    evaluationResult

  }

  def crossvalidate(k:Int, allSamples:Seq[TagSample]):Seq[TrainTest] = {
    var main = Seq[TrainTest]()

    allsamples.groupBy(_.domain).foreach{case(domain, samples) => {

      val positives = samples.filter(!_.isNegative)
      val negatives = samples.filter(_.isNegative)
      val distri = positives.length.toDouble / k

      if(distri > 0){
        main = crossUpdate(main, crossvalidateAll(k, positives))
        main = crossUpdate(main, crossvalidateAll(k, negatives))
      }

    }}

    main
  }

  def crossUpdate(main:Seq[TrainTest], crr:Seq[TrainTest]):Seq[TrainTest]={
    if(main.length == crr.length){
      main.zip(crr).foreach{case(m, c)=> m.train = m.train ++ c.train; m.test ++= c.test}
      main
    }
    else{
      crr
    }
  }

  def crossvalidateAll(k: Int, crrDomainSamples: Seq[TagSample]): Seq[TrainTest] = {

    println("Splitting dataset for cross-validation")
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
      val testSeq = splitSeqs(i)
      val trainSeq = newRange.flatMap(splitSeqs(_)).toSeq
      trainTestSeq :+= TrainTest(trainSeq, testSeq)
    }

    trainTestSeq
  }


}
