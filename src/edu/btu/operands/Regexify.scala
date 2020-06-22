package edu.btu.operands

import edu.btu.search.SinglePositiveExact

import scala.util.control.Breaks


object Regexify {

  val largeSpaceText = "                                                                                                                                                                                                       "

  val or = "|"
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


  def toOrRegex(source: RegexNodeIndex, target: RegexNodeIndex): String = {

    if (source.equalsByValue(target)) source.matchValue
    else if (source.equalsByGroup(target)) source.matchGroup
    else if (source.canMatch(target)) source.matchGroup
    else if (target.canMatch(source)) target.matchGroup
    else source.matchGroup + "|" + target.matchGroup
  }

  def toOrNode(indice:Int) : RegexNodeIndex={
    RegexNodeIndex(0, RegexOp(Regexify.or))
  }
  def toSeqNode(indice:Int) : RegexNodeIndex={
    RegexNodeIndex(indice, RegexOp(Regexify.seq))
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

  def continuousGrouping(sequence: String): RegexNodeIndex = {
    val directSequence = direct(sequence)
    continousGrouping(directSequence)
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

  def continousGrouping(mainNode: RegexNodeIndex): RegexNodeIndex = {

    val elems = mainNode.elems;
    //mainNode.regexOp.matchTxt, null,
    var nRegexNodeIndex = RegexNodeIndex(mainNode.indice, RegexOp(Regexify.seq), Seq())
      .setMatchTxt(mainNode.matchTxt);

    var i = 0
    val breaking = Breaks

    breaking.breakable {
      while (i < elems.length) {

        val crrGroup = elems(i).matchGroup
        var count = 0;
        var j = i + 1;
        var nxtTxt = elems(j).matchTxt
        var nxtVal = ""

        while (j < elems.length && nxtTxt.matches(crrGroup)) {
          nxtTxt = elems(j).matchTxt
          j += 1;
          count += 1;
        }

        if (count > 0 && j == elems.length) {
          nRegexNodeIndex = nRegexNodeIndex.add(constructByCount(elems.slice(i, j), crrGroup));
          i = j;
        }
        else if (count > 0) {
          nRegexNodeIndex = nRegexNodeIndex.add(constructByCount(elems.slice(i, i + count), crrGroup));
          i = j - 1
        }
        else if (count == 0) {
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
    else if (char=='"' || char == '+' || char == '-' || char == '?' || char == '!' || char == '.' || char == ':' || char == ';' || char == '$' || char == '^' || char == '[' || char == ']' || char == '(' || char == ')' || char == '{' || char == '}' || char == '=' || char == '*') "\\" + char.toString
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


    for (i <- 1 until seq.length) {
      mvalue = mvalue + mkStr + seq(i)._1
      mgroup = mgroup + mkStr + seq(i)._2

      if (parentOp.equals(Regexify.or) || parentOp.equals(Regexify.orBracket)) mtxt = seq(i)._3
      else mtxt = mtxt + mkStr + seq(i)._3
    }

    (toRegex(mvalue, parentOp, parentOpCount), toRegex(mgroup, parentOp, parentOpCount), mtxt)
  }

  def toRegex(regex: String, opName: String, opCount: Int): String = {
    if (regex.contains("|") && (Regexify.or.equals(opName) || Regexify.seq.equals(opName))) "(" + regex + ")"
    else if (Regexify.or.equals(opName) || Regexify.seq.equals(opName)) regex
    else if (Regexify.orBracket.equals(opName)) "[" + regex + "]"
    else if (Regexify.bracketCount.equals(opName)) "[" + regex + s"]{${opCount}}"
    else regex
  }

  def specialize(crrRegex:String):String={
    crrRegex.replaceAll("\"", "\\\"")
  }


  def toRegex(RegexNodeIndex: RegexNodeIndex): (String, String, String) = {

    RegexNodeIndex match {

      case node: RegexNodeIndex => {

        val matchGroup = node.symMatchGroup()
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

    println(validate("<div class=\"dygtag-slot dygtag-site-top dygtag-ldb3 dygtag-unfilled\">"))
    println(validate("<div class=\"dygtag-slot dygtag-popup dygtag-outofpage dygtag-ins dygtag-unfilled\">"))
    println(print("<div class=\"dygtag-slot dygtag-site-top dygtag-ldb3 dygtag-unfilled\">"))
    println(print("<div class=\"dygtag-slot dygtag-popup dygtag-outofpage dygtag-ins dygtag-unfilled\">"))

  }
}