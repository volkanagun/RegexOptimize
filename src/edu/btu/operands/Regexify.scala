package edu.btu.operands

import java.util.regex.Pattern

import edu.btu.search.SinglePositiveExact
import edu.btu.task.evaluation.ExperimentParams

import scala.util.Random
import scala.util.control.Breaks


object Regexify {

  val largeSpaceText = "                                                                                                                                                                                                       "

  val or = "|"
  val ornegate = "|!?"
  val negate = "not"
  val orgroup = "(|)"
  val oneplus = "+"
  val star = "(*)"
  val any = "."
  val anyplus = ".+"
  val anystar = ".*"
  val orBracket = "[]"
  val bracketCount = "{}"
  val orbracketCount = "[]{}"
  val orbracketOptional = "[]{}?"
  val seqrepeat = "()+"
  val group = "()"

  val seq = "seq"
  val seqor = "seqor"

  val and = "&"
  val until = "?u"
  val optional = "?o"
  val letter = "\\p{L}"
  val upchar = "\\p{javaUpperCase}"
  val punct = "\\p{Punct}"
  val space = "\\s"

  val lowchar = "\\p{javaLowerCase}"
  val digit = "\\d"
  val range = "[a-z]"

  val groupings = Seq(letter, upchar, punct, space, lowchar, digit)/*.map(item=>
    item.replaceAll("\\{","\\\\\\{")
    .replaceAll("\\}","\\\\}")
    .replaceAll("\\\\", "\\\\")
    .replaceAll("\\[","\\\\[")
    .replaceAll("\\]","\\\\]"))*/

  val rnd = new Random(1121711)


  def toOrRegex(source: RegexNodeIndex, target: RegexNodeIndex): String = {

    if (source.equalsByValue(target)) source.matchValue
    else if (source.equalsByGroup(target)) source.matchGroup
    else if (source.canMatch(target)) source.matchGroup
    else if (target.canMatch(source)) target.matchGroup
    else source.matchGroup + "|" + target.matchGroup
  }

  def toOrNode(indice: Int): RegexNodeIndex = {
    RegexNodeIndex(indice, RegexOp(Regexify.or))
  }

  def toOrNode(indice: Int, elems: Seq[RegexNodeIndex]): RegexNodeIndex = {
    RegexNodeIndex(indice, RegexOp(Regexify.or), elems)
  }

  def toOptNode(indice: Int, elems: Seq[RegexNodeIndex]): RegexNodeIndex = {
    RegexNodeIndex(indice, RegexOp(Regexify.optional), elems)
  }

  def toOrNegateNode(indice: Int, elems: Seq[RegexNodeIndex]): RegexNodeIndex = {
    RegexNodeIndex(indice, RegexOp(Regexify.ornegate), elems)
  }

  def toSeqNode(indice: Int): RegexNodeIndex = {
    RegexNodeIndex(indice, RegexOp(Regexify.seq))
  }

  def toSeqNode(indice: Int, element:RegexNodeIndex): RegexNodeIndex = {
    RegexNodeIndex(indice, RegexOp(Regexify.seq), Seq(element))
  }
  def toSeqNode(indice: Int, elements:Seq[RegexNodeIndex]): RegexNodeIndex = {
    RegexNodeIndex(indice, RegexOp(Regexify.seq),elements)
  }

  def toOrExactRegex(source: RegexNodeIndex, target: RegexNodeIndex): String = {
    if (source.equalsByValue(target)) source.matchValue
    else source.matchValue + "|" + target.matchValue
  }


  /*def orGroupLeft(mainNode: RegexNodeIndex, targetNode: RegexNodeIndex): RegexNodeIndex = {
    RegexNodeIndex(OrGroupOp(mainNode.matchTxt, null, Regexify.or, -1, -1), mainNode.elems :+ targetNode)
      .regexify()
  }

  def orGroupRight(mainNode: RegexNodeIndex, targetNode: RegexNodeIndex): RegexNodeIndex = {
    RegexNodeIndex(OrGroupOp(mainNode.matchTxt, null, Regexify.or, -1, -1), targetNode.elems :+ mainNode)
      .regexify()
  }

  def orGroup(mainNode: RegexNodeIndex, targetNode: RegexNodeIndex): RegexNodeIndex = {
    RegexNodeIndex(OrGroupOp(mainNode.matchTxt, null, Regexify.or, -1, -1), Seq(mainNode, targetNode))
      .regexify()
  }

  def orContentGroup(mainNode: RegexNodeIndex, targetNode: RegexNodeIndex): RegexNodeIndex = {
    RegexNodeIndex(OrGroupOp(mainNode.matchTxt, null, Regexify.or, -1, -1), mainNode.elems ++ targetNode.elems)
      .regexify()
  }

  def optionalGroup(mainNode: RegexNodeIndex, optionalNode: RegexNodeIndex): RegexNodeIndex = {
    RegexNodeIndex(OrGroupOp(mainNode.matchTxt, null, Regexify.optional, 1, -1), Seq(mainNode, optionalNode))
  }*/

  def spaceText(count: Int): String = {
    largeSpaceText.substring(0, count)
  }


  def optionalSequence(seq: Seq[RegexNodeIndex]): RegexNodeIndex = {
    val rvalue = "(" + seq.map(rnode => "(" + rnode.matchValue + ")") + "?)"
    val tvalue = seq.map(rnode => rnode.matchTxt).mkString("")

    RegexNodeIndex(0, RegexOp(optional), seq).setMatchTxt(tvalue).setMatchValue(rvalue)
      .setMatchGroup(anyplus)
  }

  def directSequence(seq: Seq[RegexNodeIndex]): RegexNodeIndex = {
    val rvalue = seq.map(rnode => "(" + rnode.matchValue + ")").mkString("")
    val tvalue = seq.map(rnode => rnode.matchTxt).mkString("")

    RegexNodeIndex(0, RegexOp(Regexify.seq), seq)
      .setMatchTxt(tvalue)
      .setMatchValue(rvalue)
  }

  def optionalSequence(leftList: Seq[RegexNodeIndex], intersectList: Seq[RegexNodeIndex], rightList: Seq[RegexNodeIndex]): RegexNodeIndex = {
    val leftNode = optionalSequence(leftList)
    val rightNode = optionalSequence(rightList)
    val resultSeq = Seq(leftNode, directSequence(intersectList), rightNode)

    directSequence(resultSeq)
  }

  def continousGrouping(sequence: String): Seq[RegexNodeIndex] = {
    val directSequence = direct(sequence)
    val grouping = continousGrouping(directSequence)
    /*val mainNode = toSeqNode(0, grouping)*/
    //val simple = groupSimplify(grouping)
    Seq(grouping)
  }

  def randomizedGrouping(sequence: String): Seq[RegexNodeIndex] = {
    val currentNode = continousGrouping(sequence).head
    currentNode.randomize(ExperimentParams.rndElemLength).map(_.continuous())
  }

  def groupSimplify(regexNodeIndex: RegexNodeIndex): RegexNodeIndex = {

    if (regexNodeIndex.isEmpty()) {
      regexNodeIndex.matchValue = regexNodeIndex.matchGroup
    }
    else if (!regexNodeIndex.isEmpty()) {
      regexNodeIndex.elems.foreach(groupSimplify(_))
      regexNodeIndex.matchGroup = regexNodeIndex.matchGroup + s"{${regexNodeIndex.elems.length}}"
      regexNodeIndex.matchValue = regexNodeIndex.matchGroup
    }

    regexNodeIndex
  }

  //simplify grouping
  def groupingSimplify(regexNodeIndex: RegexNodeIndex): RegexNodeIndex = {

    if (!regexNodeIndex.isEmpty()) {

      val matchingElems = regexNodeIndex.elems.takeWhile(elem => elem.matchValue.matches(regexNodeIndex.matchGroup) && elem.opName().equals(regexNodeIndex.opName()))
      if (!matchingElems.isEmpty) {

        val nnode = RegexNodeIndex(regexNodeIndex.maxDex, RegexOp(regexNodeIndex.regexOp.name), matchingElems)

        nnode.setMatchGroup(regexNodeIndex.matchGroup + s"{${matchingElems.length}}")
        nnode.setMatchTxt(matchingElems.map(_.matchTxt).mkString(""))

        val nelems = nnode +: regexNodeIndex.elems.filter(elem => !matchingElems.contains(elem))
        val nmatchValue = nelems.map(subelem => subelem.matchValue).mkString("")

        regexNodeIndex.elems = nelems
        regexNodeIndex.setMatchValue(nmatchValue)
        regexNodeIndex.setMatchGroup(nmatchValue)

      }
      else {
        regexNodeIndex.elems = regexNodeIndex.elems.map(subelem => groupingSimplify(subelem))
        val nmatchValue = regexNodeIndex.elems.map(subelem => subelem.matchValue).mkString("")

        regexNodeIndex.setMatchValue(nmatchValue)
        regexNodeIndex.setMatchGroup(nmatchValue)

      }

    }
    else {
      regexNodeIndex.matchValue = regexNodeIndex.matchGroup
    }
    regexNodeIndex.elems.foreach(subelem => subelem.setMatchValue(subelem.matchGroup))
    regexNodeIndex

  }

  def constructByCount(elems: Seq[RegexNodeIndex], matchGroup: String): RegexNodeIndex = {
    val nxtMatch = elems.map(_.matchTxt).mkString("")
    val nxtValue = elems.map(_.matchValue).mkString("")
    val nregexOp = RegexOp(Regexify.seq, elems.length)
    RegexNodeIndex(0, nregexOp, elems).setMatchTxt(nxtMatch)
      .setMatchGroup(matchGroup)
      .setMatchValue(nxtValue)
  }


  /*
  Generalize reduces the pattern size, creates a unique pattern if possible
  */
  def searchPositives(nodes: Seq[RegexNodeIndex]): Seq[Cell] = {
    if (nodes.length == 1) {
      val newNode = nodes.head.simplify()
      Seq(newNode.toCell())
    }
    else {
      val searchMethod = new SinglePositiveExact()

      val paths = searchMethod.addPositiveNodes(nodes).searchDirectional()
      if (paths.isEmpty) Seq()
      else paths.head.cells
    }
  }



  //find inner matches
  def continousOrGrouping(mainNode:RegexNodeIndex) : RegexNodeIndex = {

    val groupedElems = mainNode.elems.map(node=> continousGrouping(node))
    var indexedElems = groupedElems.zipWithIndex

    val matchElems = groupedElems.map(crrNode=> indexedElems.filter{case(subElem, in) => subElem.matchesByInfGroup(crrNode)})
      .zipWithIndex.sortBy(pair=> pair._1.length)
      .reverse

    var i = 0
    var seq = Seq[RegexNodeIndex]()
    while(!indexedElems.isEmpty) {

      //take matches of i
      val (matches, nodei) = matchElems(i)
      val matchSizes = matches.map(_._1.matchValue.length)

      val minSize = matchSizes.min
      val maxSize = matchSizes.max
      val crrNode = groupedElems(nodei)
      val newNode = toSeqNode(crrNode.maxDex, matches.map(_._1))
      val newGroup = crrNode.toCountGroup(minSize, maxSize)
      newNode.setMatchValue(crrNode.matchValue)
        .setMatchGroup(newGroup).setMatchTxt(crrNode.matchTxt)

      seq :+= newNode
      indexedElems = indexedElems.filter(pair=> !matches.map(_._2).contains(pair._2))
    }


    toOrNode(mainNode.maxDex, seq)

  }

  def continousGrouping(mainNode: RegexNodeIndex): RegexNodeIndex = {

    val elems = mainNode.elems;
    var nRegexNodeIndex = RegexNodeIndex(mainNode.maxDex, RegexOp(Regexify.seq), Seq())
      .setMatchTxt(mainNode.matchTxt);

    var i = 0
    val breaking = Breaks

    breaking.breakable {
      while (i < elems.length) {

        val crrGroup = elems(i).matchGroup
        var count = 0;
        var j = i + 1;
        var nxtTxt: String = null

        while (j < elems.length && (nxtTxt == null || nxtTxt.matches(crrGroup))) {
          nxtTxt = elems(j).matchTxt
          j += 1;
          count += 1;
        }

        if (j == elems.length && (nxtTxt == null || nxtTxt.matches(crrGroup))) {
          nRegexNodeIndex = nRegexNodeIndex.add(constructByCount(elems.slice(i, j), crrGroup));
          i = j;
        }
        else if (count > 1) {
          nRegexNodeIndex = nRegexNodeIndex.add(constructByCount(elems.slice(i, i + count), crrGroup));
          i = j - 1
        }
        else if (count >= 0) {
          nRegexNodeIndex = nRegexNodeIndex.add(elems(i));
          i = i + 1
        }
      }
    }

    nRegexNodeIndex.regexify()

  }


  def direct(sequence: String): RegexNodeIndex = {

    val elements = sequence.zipWithIndex.map { case (char, index) => (index, char.toString, direct(char), group(char)) }.map {
      case (index, char, value, group) => {
        RegexNodeIndex(index, RegexOp(Regexify.seq))
          .setMatchTxt(char)
          .setMatchValue(value)
          .setMatchGroup(group)
      }
    }

    RegexNodeIndex(0, RegexOp(Regexify.seq), elements)
  }

  def direct(elements: Seq[RegexNodeIndex]): RegexNodeIndex = {
    var matchValue = ""
    var matchText = ""

    elements.foreach(node => {
      matchValue += node.matchValue
      matchText += node.matchTxt

    })

    RegexNodeIndex(0, RegexOp(seq)).setMatchValue(matchValue)
      .setMatchTxt(matchText)
  }

  def direct(char: Char): String = {

    if (char.isLetterOrDigit) char.toString
    else if (char == ' ') "\\s"
    else if (char == '"' || char == '+' || char == '-' || char == '?' || char == '!' || char == '.' || char == ':' || char == ';' || char == '$' || char == '^' || char == '[' || char == ']' || char == '(' || char == ')' || char == '{' || char == '}' || char == '=' || char == '*') "\\" + char.toString
    else char.toString

  }

  def group(char: Char): String = {

    if (char.isUpper) upchar
    else if (char.isLower) lowchar
    else if (char.isDigit) digit
    else if (char.isLetter) letter
    else if (char.isWhitespace) space
    else if (char == '[' || char == ']' || char == '(' || char == ')' || char == '{' || char == '}' || char == '?' || char == '!' || char == '.' || char == ':' || char == ';') punct
    else if (char == '+' || char == '-') "\\" + char
    else char.toString
  }


  def toNegateRegex(elems: Seq[RegexNodeIndex], parentOp: String, parentOpCount: Int = 0, mkStr:String = ""): (String, String, String) ={
    val ornegs = elems.filter(_.notNode)
    val ors = elems.filter(! _.notNode)

    val (cvalue, cgroup, ctxt) = toRegex(ors, parentOp, parentOpCount, mkStr)
    val (nvalue, ngroup, ntxt) = toRegex(ornegs, parentOp, parentOpCount, mkStr)

    val lgroup = s"(?!${ngroup})(${cgroup})"
    val lvalue = s"(?!${nvalue})(${cvalue})"
    (lvalue, lgroup, ctxt)
  }

  /**
   * Match group, value, txt
   *
   * @param elems
   * @param mkStr
   * @return
   */
  def toRegex(elems: Seq[RegexNodeIndex], parentOp: String, parentOpCount: Int = 0, mkStr: String = ""): (String, String, String) = {

    val seq = elems.map(toRegex(_))
    val head = seq.head
    var (mvalue, mgroup, mtxt) = (head._1, head._2, head._3);
    var negtxt = ""

    var groupCount = 1

    var pregroup = mgroup
    mgroup = ""
    for (i <- 1 until seq.length) {
      val newValue = seq(i)._1
      val newGroup = seq(i)._2

      mvalue = mvalue + mkStr + newValue

      if (newGroup.equals(pregroup)) groupCount += 1
      else if (groupCount > 1) {
        mgroup = mgroup + pregroup + s"{${groupCount}}"
        pregroup = newGroup
        groupCount = 1
      }
      else {
        mgroup = mgroup + pregroup
        pregroup = newGroup
      }

      if (parentOp.equals(Regexify.or) || parentOp.equals(Regexify.orBracket)) mtxt = seq(i)._3
      else mtxt = mtxt + mkStr + seq(i)._3
    }

    if (groupCount > 1) mgroup = mgroup + pregroup + s"{${groupCount}}"
    else mgroup = mgroup + pregroup
    (toRegex(mvalue, parentOp, parentOpCount), toRegex(mgroup, parentOp, parentOpCount), mtxt)
  }


  def flipCoin(prev: String, crr:String): Boolean = {
    if(crr.equals("(")||crr.equals(")")||crr.equals("{")||crr.equals("[")||crr.equals("]")||crr.equals("}") || crr.equals("\\") || crr.equals("+") || crr.matches("\\d")) false
    else if(prev.equals("\\") && crr.matches("(\\p{L}|\\p{Punct})")) false
    else if((prev.equals("p")||prev.startsWith("L")) && crr.matches("\\p{Punct}")) false
    else if(prev.matches("\\p{Punct}") && (crr.startsWith("L")||crr.startsWith("P"))) false
    else rnd.nextBoolean()
  }

  def toRandom(regex:String):Set[String] = {
    randomize(regex, 5).map(crr=>{
      replace(crr)
    })
  }

  // do not randomize groups
  def randomize(regex: String): String = {
    val map = Range(0, regex.length)
      .map(i => (i, flipCoin(regex(math.max(i-1, 0)).toString, regex(i).toString)))
      .filter(pair=> pair._2)
      .map(pair=> pair._1->group(regex(pair._1))).toMap

    Range(0, regex.length).map(i=> if(map.contains(i)) map(i) else regex(i))
      .mkString("")
  }

  def randomize(regex: String, repeat: Int): Set[String] = {
    var seq = Set[String]()
    for (i <- 0 until repeat) {
      var result = randomize(regex)
      seq += result
    }

    seq
  }

  def replace(regex:String):String={
    var newregex = regex;
    groupings.foreach(crr=> {
      newregex = replace(newregex, crr)

    })

    newregex
  }



  def replace(regex:String, grouping:String) : String={
    val search = grouping
    val regexSingle = "(" + Pattern.quote(search) + ")"
    val regexMulti = "(" + Pattern.quote(search) + ")+"
    val pattern = Pattern.compile(regexMulti)
    val matcher = pattern.matcher(regex)

    var result = "";
    var start = 0
    while (matcher.find()){
      val matchCrr = matcher.group()
      val st = matcher.start()
      val en = matcher.end()
      val size = regexSingle.r.findAllMatchIn(matchCrr).size
      if(size > 1) result += regex.substring(start, st) + search + s"{${size}}"
      else result += regex.substring(start, st) + search
      start = en
    }

    result += regex.substring(start)
    result
  }



  def toRegex(regex: String, opName: String, opCount: Int): String = {
    if (regex.contains("|") && (Regexify.or.equals(opName) || Regexify.seq.equals(opName))) "(" + regex + ")"
    else if (Regexify.or.equals(opName) || Regexify.seq.equals(opName)) regex
    else if (Regexify.orBracket.equals(opName)) "[" + regex + "]"
    else if (Regexify.optional.equals(opName)) "(" + regex + "?)"
    else if (Regexify.bracketCount.equals(opName)) "[" + regex + s"]{${opCount}}"
    else regex
  }

  def specialize(crrRegex: String): String = {
    crrRegex.replaceAll("\"", "\\\"")
  }


  def toRegex(regexNodeIndex: RegexNodeIndex): (String, String, String) = {

    regexNodeIndex match {

      case node: RegexNodeIndex => {

        val matchGroup = node.cntzMatchGroup()
        val matchValue = node.symMatchValue()
        val matchTxt = node.symMatchTxt()

        val opname = node.regexOp.name
        val opcount = node.regexOp.count
        val elems = node.elems

        if (opname != null && elems.length > 0) {

          if (opname.equals(seq)) {
            toRegex(elems, opname)
          }
          else if (opname.equals(or)) toRegex(elems, opname, opcount, "|")
          else if (opname.equals(ornegate)) toNegateRegex(elems, opname, opcount, "|")
          else if (opname.equals(orBracket)) toRegex(elems, opname)
          else if (opname.equals(bracketCount)) toRegex(elems, opname, opcount)
          else if (opname.equals(optional)) toRegex(elems, opname, opcount)
          else throw new Exception(s"Operation group not found for group: ${opname} and value: ${opname}")
        }
        else if(opname.equals(negate) && !matchValue.startsWith("(?!")){
          (s"(?!${matchValue})", s"(?!${matchGroup})", s"(?!${matchTxt})")
        }
        else {
          (matchValue, matchGroup, matchTxt)
        }
      }
    }
  }


  def toRandomRegex(regexNodeIndex: RegexNodeIndex): (String, String, String) = {

    regexNodeIndex match {

      case node: RegexNodeIndex => {

        val matchGroup = node.cntzMatchGroup()
        val matchValue = node.symMatchValue()
        val matchTxt = node.symMatchTxt()

        val opname = node.regexOp.name
        val opcount = node.regexOp.count
        val elems = node.elems

        if (opname != null && elems.length > 0) {

          if (opname.equals(seq)) {
            toRegex(elems, opname)
          }
          else if (opname.equals(or)) toRegex(elems, opname, opcount, "|")
          else if (opname.equals(orBracket)) toRegex(elems, opname)
          else if (opname.equals(bracketCount)) toRegex(elems, opname, opcount)
          else if (opname.equals(optional)) toRegex(elems, opname, opcount)
          else throw new Exception(s"Operation group not found for group: ${opname} and value: ${opname}")

        }
        else {
          (matchValue, matchGroup, matchTxt)
        }
      }
    }
  }

  def validate(text: String): String = {
    val RegexNodeIndex = direct(text)
    "Validated==>" + toRegex(RegexNodeIndex)._1.toString()
  }

  def print(text: String): String = {
    val RegexNodeIndex = direct(text)
    "Print==>" + RegexNodeIndex.toString(8)
  }

  def main(args: Array[String]): Unit = {

    /*println(validate("<div class=\"dygtag-slot dygtag-site-top dygtag-ldb3 dygtag-unfilled\">"))
    println(validate("<div class=\"dygtag-slot dygtag-popup dygtag-outofpage dygtag-ins dygtag-unfilled\">"))
    println(print("<div class=\"dygtag-slot dygtag-site-top dygtag-ldb3 dygtag-unfilled\">"))
    println(print("<div class=\"dygtag-slot dygtag-popup dygtag-outofpage dygtag-ins dygtag-unfilled\">"))*/

    val str = "aaa\\d\\d\\d\\p{L}\\p{L}"
    println(toRandom(str).mkString("\n"))

  }
}