package edu.btu.task.evaluation

import java.io._

import edu.btu.operands.{RegexGenerator, RegexString}
import edu.btu.task.tagmatch.TagSample

import scala.xml.{NodeSeq, XML}

class ExperimentParams extends Serializable {

  val singleExact = "SINGLE-EXACT"
  val singleApprox = "SINGLE-APPROX"
  val multiExact = "MULTI-EXACT"
  val multiApprox = "MULTI-APPROX"
  val singleNGRAM = "SINGLE-NGRAM"
  val multiNGRAM = "MULTI-NGRAM"

  val sampleSize = "SAMPLE SIZE"
  val relevantCount = "POSITIVE COUNT"
  val irrelevantCount = "NEGATIVE COUNT"
  val sampleDomainRatio = "SAMPLE DOMAIN RATIO"
  val fileSize = "FILE SIZE"
  val fileRatio = "FILE RATIO"
  val fileDomainRatio = "FILE DOMAIN RATIO"
  val fileSampleRatio = "FILE SAMPLE RATIO"

  //search with multiple regexes
  val regexMulti = "REGEX-MULTI"
  //search with single regex construct longer regex
  val regexSingle = "REGEX-SINGLE"

  var doNGramFilter = true

  //use filenames or domains
  val samplesFromDomain = "DOMAIN-SAMPLES"
  val samplesFromFilename = "FILENAME-SAMPLES"

  var domainNotFile = true

  //cross-validation folds
  var k = 3
  //combine maximum 3 regexes
  var maxCombineSize = 7
  //max repeat random size
  var maxRegexSize = 5
  //number of search paths increase for better accuracy
  var maxPaths = maxCombineSize * maxRegexSize
  //shuffle seeds
  var shuffleSeed = 1711
  var shuffleSeed2 = 171178

  //number of training samples per domain
  //for better accuracy increase it
  //for better efficiency decrease it
  var maxSamples = 1000000
  //ratio of regex patterns constructed by training samples that match the given sample
  //used to filter accepted regex patterns
  //low ratio increase the number of regex patterns while decreases the efficiency
  //choose -1 for ngrams
  var matchSelectRatio: Double = 0.01d

  //the ratio of common n-gram dictionary patterns for the sample
  //accept or reject the positive or negative sample pattern for n-grams
  //increasing it will reduce the number of patterns
  var patternFilterRatio: Double = 0.5
  //take n-gram samples count (top counted ngram patterns) (topCount)

  //increase it for better accuracy
  //decrease it for better efficiency
  var topCount = 100
  var ngramLength = 7
  //number of generalized patterns
  var rndElemLength = 4
  var ngramStepLength = 1

  var maxMultiDepth = 2

  var folder = "resources/img-csvs/"
  var datasets = "resources/datasets/"

  var experimentCycle = Array[String](multiNGRAM, regexMulti)

  //Filter generators
  val allMapFilename = "resources/binary/regexGen"
  val allSamplesFilename = "resources/binary/samples.bin"

  var excludedDomains = Seq[String]()
  var selectedDomains = Seq[String]()
  val paramsFilename = "resources/params.xml"
  var minimumPositiveSamples = 20


  def generationMapID(domainName: String, foldNum: Int): Int = {

    var r = 1
    r = 3 * r + foldNum
    r = 3 * r + ngramLength
    r = 3 * r + ngramStepLength
    r = 3 * r + topCount
    r = 3 * r + patternFilterRatio.hashCode()
    r = 3 * r + matchSelectRatio.hashCode()
    r = 3 * r + maxSamples
    r = 3 * r + k
    r = 3 * r + maxPaths
    r = 3 * r + maxMultiDepth
    r = 3 * r + domainName.hashCode
    r = 3 * r + experimentCycle.toSeq.hashCode()
    r = 3 * r + excludedDomains.hashCode()
    r = 3 * r + selectedDomains.hashCode()
    r = 3 * r + minimumPositiveSamples
    r = 3 * r + rndElemLength
    r

  }

  //accomplished
  def generationID(): String = {

    var r = 1
    r = 3 * r + maxCombineSize
    r = 3 * r + maxRegexSize
    r = 3 * r + maxPaths
    r = 3 * r + maxMultiDepth
    r = 3 * r + maxSamples
    r = 3 * r + k
    r = 3 * r + ngramLength
    r = 3 * r + ngramStepLength
    r = 3 * r + topCount
    r = 3 * r + patternFilterRatio.hashCode()
    r = 3 * r + matchSelectRatio.hashCode()
    r = 3 * r + experimentCycle.toSeq.hashCode()
    r = 3 * r + excludedDomains.hashCode()
    r = 3 * r + selectedDomains.hashCode()
    r = 3 * r + minimumPositiveSamples
    r = 3 * r + rndElemLength

    r = 3 * r + (if (doNGramFilter) 1 else 0)

    String.valueOf(r)

  }

  def saveXML(): this.type = {
    val rootOpen = "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n" +
      "<ROOT>\n"
    val params = paramsXML().split("\\n").filter(line => !line.contains("MAX_PATH")).mkString("\n")


    val all = rootOpen + params + "\n</ROOT>"

    new PrintWriter(paramsFilename) {
      println(all)
      close()
    }

    this
  }

  def loadXML(): this.type = {
    if (!new File(paramsFilename).exists()) return this;
    val xmlDoc = XML.load(paramsFilename)
    val elem = (xmlDoc \\ "PARAMETERS").head
    val params = elem \\ "PARAM"
    params.foreach(item => {
      val attr = item.attribute("NAME").get.head.text
      val value = item.attribute("VALUE").get.head.text
      if (attr.equals("MAX_COMBINE_SIZE")) maxCombineSize = value.toInt
      else if (attr.equals("MAX_REGEX_SIZE")) maxRegexSize = value.toInt
      /*else if (attr.equals("MAX_PATHS")) maxPaths = value.toInt*/
      else if (attr.equals("MAX_SAMPLES")) maxSamples = value.toInt
      else if (attr.equals("FOLD_SIZE")) k = value.toInt
      else if (attr.equals("NGRAM_LENGTH")) ngramLength = value.toInt
      else if (attr.equals("NGRAM_STEP_LENGTH")) ngramStepLength = value.toInt
      else if (attr.equals("MAX_MULTI_DEPTH")) maxMultiDepth = value.toInt
      else if (attr.equals("TOP_COUNT")) topCount = value.toInt
      else if (attr.equals("PATTERN_FILTER_RATIO")) patternFilterRatio = value.toDouble
      else if (attr.equals("MATCH_SELECT_RATIO")) matchSelectRatio = value.toDouble
      else if (attr.equals("EXPERIMENT_CYCLE")) experimentCycle = value.split("\\+\\+").filter(!_.isEmpty)
      else if (attr.equals("SELECTED_DOMAINS")) excludedDomains = value.split("\\+\\+").filter(!_.isEmpty)
      else if (attr.equals("NGRAM_FILTER")) doNGramFilter = value.toBoolean
      else if (attr.equals("DOMAIN_EXPERIMENT")) domainNotFile = value.toBoolean
      else if (attr.equals("MIN_POS_SAMPLES")) minimumPositiveSamples = value.toInt
      else if (attr.equals("RND_GROUPING_SIZE")) rndElemLength = value.toInt
      else {}

    })

    maxPaths = maxCombineSize * maxRegexSize

    this
  }

  def loadXML(params: NodeSeq):this.type ={
    params.foreach(item => {
      val attr = item.attribute("NAME").get.head.text
      val value = item.attribute("VALUE").get.head.text
      if (attr.equals("MAX_COMBINE_SIZE")) maxCombineSize = value.toInt
      else if (attr.equals("MAX_REGEX_SIZE")) maxRegexSize = value.toInt
      /*else if (attr.equals("MAX_PATHS")) maxPaths = value.toInt*/
      else if (attr.equals("MAX_SAMPLES")) maxSamples = value.toInt
      else if (attr.equals("FOLD_SIZE")) k = value.toInt
      else if (attr.equals("NGRAM_LENGTH")) ngramLength = value.toInt
      else if (attr.equals("NGRAM_STEP_LENGTH")) ngramStepLength = value.toInt
      else if (attr.equals("MAX_MULTI_DEPTH")) maxMultiDepth = value.toInt
      else if (attr.equals("TOP_COUNT")) topCount = value.toInt
      else if (attr.equals("PATTERN_FILTER_RATIO")) patternFilterRatio = value.toDouble
      else if (attr.equals("MATCH_SELECT_RATIO")) matchSelectRatio = value.toDouble
      else if (attr.equals("EXPERIMENT_CYCLE")) experimentCycle = value.split("\\+\\+").filter(!_.isEmpty)
      else if (attr.equals("SELECTED_DOMAINS")) excludedDomains = value.split("\\+\\+").filter(!_.isEmpty)
      else if (attr.equals("NGRAM_FILTER")) doNGramFilter = value.toBoolean
      else if (attr.equals("DOMAIN_EXPERIMENT")) domainNotFile = value.toBoolean
      else if (attr.equals("MIN_POS_SAMPLES")) minimumPositiveSamples = value.toInt
      else if (attr.equals("RND_GROUPING_SIZE")) rndElemLength = value.toInt
      else {}
    })

    maxPaths = maxCombineSize * maxRegexSize
    this
  }

  def paramsXML(): String = {
    "<PARAMETERS>\n" +
      "<!--max paths is equal to maxCombineSize * maxRegexSize-->\n" +
      "<PARAM NAME=\"MAX_COMBINE_SIZE\" VALUE=\"" + maxCombineSize + "\"/>\n" +
      "<!--max regex size is the final regex size for single attribute value-->\n" +
      "<PARAM NAME=\"MAX_REGEX_SIZE\" VALUE=\"" + maxRegexSize + "\"/>\n" +
      /*"<PARAM NAME=\"MAX_PATHS\" VALUE=\"" + maxPaths + "\"/>\n" +*/
      "<PARAM NAME=\"MAX_MULTI_DEPTH\" VALUE=\"" + maxMultiDepth + "\"/>\n" +
      "<PARAM NAME=\"MAX_SAMPLES\" VALUE=\"" + maxSamples + "\"/>\n" +
      "<PARAM NAME=\"FOLD_SIZE\" VALUE=\"" + k + "\"/>\n" +
      "<PARAM NAME=\"NGRAM_LENGTH\" VALUE=\"" + ngramLength + "\"/>\n" +
      "<PARAM NAME=\"NGRAM_STEP_LENGTH\" VALUE=\"" + ngramStepLength + "\"/>\n" +
      "<!--top count is equal to input sample size-->\n" +
      "<PARAM NAME=\"TOP_COUNT\" VALUE=\"" + topCount + "\"/>\n" +
      "<PARAM NAME=\"PATTERN_FILTER_RATIO\" VALUE=\"" + patternFilterRatio + "\"/>\n" +
      "<PARAM NAME=\"MATCH_SELECT_RATIO\" VALUE=\"" + matchSelectRatio + "\"/>\n" +
      "<PARAM NAME=\"EXPERIMENT_CYCLE\" VALUE=\"" + experimentCycle.mkString("++") + "\"/>\n" +
      "<PARAM NAME=\"SELECTED_DOMAINS\" VALUE=\"" + excludedDomains.mkString("++") + "\"/>\n" +
      "<PARAM NAME=\"NGRAM_FILTER\" VALUE=\"" + doNGramFilter + "\"/>\n" +
      "<PARAM NAME=\"DOMAIN_EXPERIMENT\" VALUE=\"" + domainNotFile + "\"/>\n" +
      "<PARAM NAME=\"MIN_POS_SAMPLES\" VALUE=\"" + minimumPositiveSamples + "\"/>\n" +
      "<PARAM NAME=\"RND_GROUPING_SIZE\" VALUE=\"" + rndElemLength + "\"/>\n" +
      "</PARAMETERS>"
  }

  def mapFilename(id: Int): String = {
    allMapFilename + s"-${id}.bin"
  }

  def mapBinaryExists(id: Int): Boolean = {
    new File(mapFilename(id)).exists()
  }

  def samplesBinaryExists(): Boolean = {
    new File(allSamplesFilename).exists()
  }

  def loadSamples(samples: => Seq[TagSample]): Seq[TagSample] = {
    if (samplesBinaryExists()) {
      try {
        val allsamples = loadObject[Seq[TagSample]](allSamplesFilename)
        if(allsamples!=null)  allsamples
        else saveSamples(samples)
      }catch{
        case e:Exception => {
          saveSamples(samples)
        }
      }
    }
    else {
      saveSamples(samples)
    }
  }

  def saveSamples(samples: Seq[TagSample]): Seq[TagSample] = {
    saveObject[Seq[TagSample]](allSamplesFilename, samples)
  }

  def saveGenerator(regexGenMap: Array[(String, Seq[RegexGenerator])], id: Int): Array[(String, Seq[RegexGenerator])] = {
    saveObject[Array[(String, Seq[RegexGenerator])]](mapFilename(id), regexGenMap)
  }

  def loadGenerator(regexGenMap: => Array[(String, Seq[RegexGenerator])], id: Int, force: Boolean = true): Array[(String, Seq[RegexGenerator])] = {
    if (mapBinaryExists(id)) {
      val obj = loadObject[Array[(String, Seq[RegexGenerator])]](mapFilename(id))
      if (obj == null || force) {
        saveObject(mapFilename(id), regexGenMap)
      }
      else obj
    }
    else {
      saveObject(mapFilename(id), regexGenMap)
    }
  }

  def loadObject[A >: Null](filename: String): A = {
    try {
      val objin = new ObjectInputStream(new FileInputStream(filename))
      val value = objin.readObject().asInstanceOf[A]
      objin.close()
      value
    }
    catch {
      case _ => {
        println("Error in filename: " + filename)
        null
      }
    }
  }


  def saveObject[A](filename: String, obj: A): A = {
    val objin = new ObjectOutputStream(new FileOutputStream(filename))
    objin.writeObject(obj)
    objin.close()
    obj
  }


  def filterGenerator(regexGenMap: => Array[(String, Seq[RegexGenerator])]): Array[(String, Seq[RegexGenerator])] = {

    println("Filter generators...")
    regexGenMap.foreach { case (_, generators) => {
      if (ExperimentParams.doNGramFilter) generators.map(generator => generator.filterSlice())
    }
    }

    regexGenMap.map { case (name, generators) => {
      val nonEmptyGens = generators.filter(regexGenerator => regexGenerator.filter())
      (name, nonEmptyGens)
    }
    }.filter(!_._2.isEmpty)

  }

  def combineGenerator(mapping: => Array[(String, Seq[RegexGenerator])]): Array[(String, Seq[RegexGenerator])] = {

    println("Combining generators...")

    //group by name
    //use single for all combine
    //use multi for first and last separate combine
    if (ExperimentParams.isSingle()) {
      mapping.map { case (name, seq) => {
        if (!seq.isEmpty) {
          val head = seq.head
          val tail = seq.tail.foreach(reg => {
            head.positives ++= reg.positives
            head.negatives ++= reg.negatives
          })

          (name, Seq(head))
        }
        else {

          (name, Seq())

        }
      }
      }

    }
    else {
      mapping.map { case (name, seq) => {
        if (!seq.isEmpty) {
          val head = seq.head
          val last = seq.last

          (name, Seq(head, last))
        }
        else {
          (name, Seq())
        }
      }
      }
    }
  }

  def regexGenerator(training: => Set[TagSample]): Map[String, Seq[RegexGenerator]] = {

    if (experimentCycle.contains(singleExact)) {
      val positiveSamples = training.filter(!_.isNegative)
      val posMap = positiveSamples.flatMap(tg => tg.positiveRegex.multimap.flatMap { case (tag, set) => set.map(item => tag -> item) })
        .groupBy(_._1).mapValues(_.map(_._2)).mapValues(positiveCases => Seq(RegexString.applyExact(this, positiveCases)))
      posMap
    }
    else if (experimentCycle.contains(singleApprox)) {
      val positiveSamples = training.filter(!_.isNegative)
      val posMap = positiveSamples.flatMap(tg => tg.positiveRegex.multimap.flatMap { case (tag, set) => set.map(item => tag -> item) })
        .groupBy(_._1).mapValues(_.map(_._2)).mapValues(positiveCases => Seq(RegexString.applyApproximate(this, positiveCases)))
      posMap
    }
    /*else if (experimentCycle.contains(singleExact) && experimentCycle.contains(regexMulti)) {
      val positiveSamples = training.filter(!_.isNegative)
      val posMap = positiveSamples.flatMap(tg => tg.positiveRegex.multimap.flatMap { case (tag, set) => set.map(item => tag -> item) })
        .groupBy(_._1).mapValues(_.map(_._2)).mapValues(positiveCases => Seq(RegexString.applyExact(positiveCases)))

      /*val negativeMap = training.flatMap(tg => tg.negativeRegex.multimap.flatMap { case (tag, set) => set.map(item => tag -> item) })
        .groupBy(_._1).mapValues(_.map(_._2)).mapValues(negativeCases => Seq(RegexString.applyExact(negativeCases)))*/

      posMap

    }*/
    /*else if (experimentCycle.contains(singleApprox) && experimentCycle.contains(regexMulti)) {
      val positiveSamples = training.filter(!_.isNegative)
      val posMap = positiveSamples.flatMap(tg => tg.positiveRegex.multimap.flatMap { case (tag, set) => set.map(item => tag -> item) })
        .groupBy(_._1).mapValues(_.map(_._2)).mapValues(positiveCases => Seq(RegexString.applyApproximate(positiveCases)))

      /*val negativeMap = training.flatMap(tg => tg.negativeRegex.multimap.flatMap { case (tag, set) => set.map(item => tag -> item) })
        .groupBy(_._1).mapValues(_.map(_._2)).mapValues(positiveCases => Seq(RegexString.applyApproximate(positiveCases)))*/

      posMap

    }*/
    else if (experimentCycle.contains(multiExact)) {

      val positiveSamples = training.filter(!_.isNegative)
      val negativeSamples = training.filter(_.isNegative)

      val posMap = positiveSamples.flatMap(tg => tg.positiveRegex.multimap.flatMap { case (tag, set) => set.map(item => tag -> item) })
        .groupBy(_._1).mapValues(_.map(_._2))
      val negMap = negativeSamples.flatMap(tg => tg.negativeRegex.multimap.flatMap { case (tag, set) => set.map(item => tag -> item) })
        .groupBy(_._1).mapValues(_.map(_._2))

      //generate two regexes for positive and negative
      posMap.map { case (tag, positiveCases) => (tag, positiveCases, negMap.getOrElse(tag, Set())) }
        .map { case (tag, pos, neg) => tag -> Seq(RegexString.applyExact(this, pos, neg), RegexString.applyExact(this, neg, pos)) }
        .toMap

    }
    else if (experimentCycle.contains(multiApprox)) {
      val positiveSamples = training.filter(!_.isNegative)
      val negativeSamples = training.filter(_.isNegative)


      val positiveMap = positiveSamples.flatMap(tg => tg.positiveRegex.multimap.flatMap { case (tag, set) => set.map(item => tag -> item) })
        .groupBy(_._1).mapValues(_.map(_._2))
      val negativeMap = negativeSamples.flatMap(tg => tg.negativeRegex.multimap.flatMap { case (tag, set) => set.map(item => tag -> item) })
        .groupBy(_._1).mapValues(_.map(_._2))
      //generate two regexes for positive and negative
      positiveMap.map { case (tag, positiveCases) => (tag, positiveCases, negativeMap.getOrElse(tag, Set())) }
        .map { case (tag, pos, neg) => tag -> Seq(RegexString.applyApproximate(this, pos, neg), RegexString.applyApproximate(this, neg, pos)) }.toMap

    }

    else if (experimentCycle.contains(singleNGRAM)) {

      val positiveSamples = training.filter(!_.isNegative)
      val positiveMap = positiveSamples.flatMap(tg => tg.positiveRegex.multimap.flatMap { case (tag, set) => set.map(item => tag -> item) })
        .groupBy(_._1).mapValues(_.map(_._2))
      positiveMap.map { case (tag, positiveCases) => (tag, positiveCases) }
        .map { case (tag, pos) => tag -> Seq(RegexString.applyNGram(this, pos)) }

    }
    else if (experimentCycle.contains(multiNGRAM)) {

      val positiveSamples = training.filter(!_.isNegative)
      val negativeSamples = training.filter(_.isNegative)

      val positiveMap = positiveSamples.flatMap(tg => tg.positiveRegex.multimap.flatMap { case (tag, set) => set.map(item => tag -> item) })
        .groupBy(_._1).mapValues(_.map(_._2))
      val negativeMap = negativeSamples.flatMap(tg => tg.negativeRegex.multimap.flatMap { case (tag, set) => set.map(item => tag -> item) })
        .groupBy(_._1).mapValues(_.map(_._2))

      positiveMap.map { case (tag, positiveCases) => (tag, positiveCases, negativeMap.getOrElse(tag, Set())) }
        .map { case (tag, pos, neg) => tag -> Seq(RegexString.applyNGram(this, pos, neg), RegexString.applyNGram(this, neg, pos)) }.toMap

    }
    else Map()
  }

  def isSingle(): Boolean = {
    experimentCycle.contains(singleApprox) || experimentCycle.contains(singleExact) || experimentCycle.contains(regexSingle) || experimentCycle.contains(singleNGRAM)
  }

}

object ExperimentParams extends ExperimentParams {
  def apply(): ExperimentParams = new ExperimentParams()
}
