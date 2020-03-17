package edu.btu.operands

import scala.collection.immutable.Stack
import scala.collection.mutable
import scala.util.control.Breaks


//content: a-c, min-count, max-count, exact-count
//transitivity required in methods

case class RegexOp(name:String, count:Int = -1) extends Serializable {

  var containerNode: RegexNodeIndex = null

  def setContainer(container:RegexNodeIndex):this.type ={
    this.containerNode = container
    this
  }

  def regexify(): RegexOp = {
    containerNode.regexify().regexOp
  }

  def copy() = RegexOp(name, count).setContainer(containerNode)

/*
  def copy(): RegexOp = {
    SimpleOp(matchTxt, value, group)
  }

  def toOptionalNode():RegexNode={
    RegexNode(toOptional(), containerNode.elems)
  }*/

  /*def toOptional(): RegexOp = {
    this match {
      case SimpleOp(txt, nvalue, noptype) => SimpleOp(txt, "(" + nvalue + ")?", Regexify.optional)
      case OrGroupOp(txt, nvalue, noptype, min, max) => OrGroupOp(txt, "(" + nvalue + "?)", Regexify.optional, min, max)
    }
  }


  def toRegular(): RegexOp = {
    this match {
      case op: SimpleOp => op
      case orGroupOp: OrGroupOp => orGroupOp
    }
  }

  def toUntil(): RegexOp = {
    this match {
      case SimpleOp(txt, nvalue, noptype) => SimpleOp(txt, "(" + nvalue + ")?", Regexify.optional)
      case OrGroupOp(txt, nvalue, noptype, min, max) => OrGroupOp(txt, nvalue, noptype, min, max)
    }
  }

  override def hashCode(): Int = {
    val state = Seq(group)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[RegexOp]

  override def equals(other: Any): Boolean = other match {
    case that: RegexOp =>
      (that canEqual this) &&
        group == that.group && value==that.value && matchTxt == that.matchTxt
    case _ => false
  }*/

}

case class Search(left: Seq[RegexNodeIndex], medium: Seq[RegexNodeIndex], right: Seq[RegexNodeIndex]) extends Serializable


object RegexOp {

  //construct hierarchy of combines and score them
  //combine two regular expressions by searching the minimal combination length and maximum specificity

  def split(node: RegexNodeIndex, sequence: Seq[RegexNodeIndex]): Search = {

    var leftSeq = Seq[RegexNodeIndex]()
    var mediumSeq = Seq[RegexNodeIndex]()
    var rightSeq = Seq[RegexNodeIndex]()

    var i = 0
    var flip = 0

    while (i < sequence.length) {

      val crrNode = sequence(i)

      if (flip == 0 && !crrNode.equalsByGroup(node)) {
        leftSeq :+= crrNode
      }
      else if ((flip == 0 || flip == 1) && crrNode.equalsByGroup(node)) {
        mediumSeq :+= crrNode
        flip = 1
      }
      else if ((flip == 1 || flip == 2) && !crrNode.equalsByGroup(node)) {
        rightSeq :+= crrNode
        flip = 2
      }
      else if (flip == 2) {
        rightSeq :+= crrNode
      }

      i += 1

    }

    Search(leftSeq, mediumSeq, rightSeq)
  }


  def slice(index: Int, sequence: Seq[RegexNodeIndex]): Seq[RegexNodeIndex] = {
    sequence.slice(index, sequence.length)
  }

  def slice(index: Int, end: Int, sequence: Seq[RegexNodeIndex]): Seq[RegexNodeIndex] = {
    sequence.slice(index, end)
  }

  def refresh(sequence: Seq[RegexNodeIndex], skipList: Seq[RegexNodeIndex]): Seq[RegexNodeIndex] = {
    var cnt = 0
    val breaks = Breaks

    breaks.breakable {
      for (i <- 0 until skipList.length) {
        val crrSkipNode = skipList(i)
        if (sequence(i).equalsByGroup(crrSkipNode)) cnt += 1
        else breaks.break()
      }
    }

    sequence.slice(cnt, sequence.length)
  }

/*
  def countCombine(leftNode: RegexNode, rightNode: RegexNode): String = {
    val leftOp = leftNode.regexOp
    val rightOp = rightNode.regexOp

    val leftSize = leftNode.elems.length
    val rightSize = rightNode.elems.length
    val size = "{" + math.min(leftSize, rightSize) + "," + math.max(leftSize, rightSize) + "}"
    val group = leftOp.group
    group + size
  }*/

/*

  def optionalCombine(leftOpts: Seq[RegexNode], rightOpts: Seq[RegexNode]): String = {
    if (!leftOpts.isEmpty && !rightOpts.isEmpty) {
      "(((" + leftOpts.map(rnode => rnode.regexOp.value).mkString("") + ")|(" +
        rightOpts.map(rnode => rnode.regexOp.value).mkString("") + "))?)"
    }
    else if (!leftOpts.isEmpty) {
      "(" + leftOpts.map(rnode => rnode.regexOp.value).mkString("") + "?)"
    }
    else if (!rightOpts.isEmpty) {
      "(" + rightOpts.map(rnode => rnode.regexOp.value).mkString("") + "?)"
    }
    else ""
  }
*/
/*
  def combine(mainNode: RegexNode, targetNode: RegexNode): RegexNode = {

    val m = mainNode.regexOp
    val t = targetNode.regexOp

    if (m.isInstanceOf[SimpleOp] && t.isInstanceOf[SimpleOp]) {
      if (m.value.matches(t.value)) targetNode
      else if (t.value.matches(m.value)) mainNode
      else {
        RegexNode(OrGroupOp(m.value + t.value, "(" + m.value + "|" + t.value + ")", Regexify.or, 1, 1), targetNode.elems ++ mainNode.elems)
      }
    }
    else if (m.isInstanceOf[SimpleOp] && t.isInstanceOf[OrGroupOp]) {
      val tor = t.asInstanceOf[OrGroupOp]
      if (m.value.matches(tor.value)) {
        targetNode
      }
      else {
        RegexNode(tor, targetNode.elems :+ mainNode).regexify()
      }
    }
    else if (m.isInstanceOf[OrGroupOp] && t.isInstanceOf[SimpleOp]) {
      val mor = m.asInstanceOf[OrGroupOp]
      val tsi = t.asInstanceOf[SimpleOp]
      if (m.value.matches(tsi.value)) mainNode
      else {
        RegexNode(mor, mainNode.elems :+ targetNode).regexify()
      }
    }
    else if (m.isInstanceOf[OrGroupOp] && t.isInstanceOf[SpecialOp]) {
      val mor = m.asInstanceOf[OrGroupOp]
      if (mor.value.matches(t.value)) targetNode
      else {
        Regexify.orGroupLeft(mainNode, targetNode)
      }
    }
    else if (m.isInstanceOf[SpecialOp] && t.isInstanceOf[OrGroupOp]) {
      val tor = t.asInstanceOf[OrGroupOp]
      if (tor.value.matches(t.value)) targetNode
      else {
        Regexify.orGroupRight(mainNode, targetNode)
      }
    }
    else if (m.isInstanceOf[SimpleOp] && t.isInstanceOf[SeqGroupOp]) {
      val tor = t.asInstanceOf[SeqGroupOp]
      if (m.value.matches(tor.value)) targetNode
      else {
        Regexify.orGroup(mainNode, targetNode)
      }
    }
    else if (m.isInstanceOf[SeqGroupOp] && t.isInstanceOf[SimpleOp]) {
      val mor = m.asInstanceOf[SeqGroupOp]
      if (t.value.matches(mor.value)) {
        targetNode
      }
      else {
        Regexify.orGroup(mainNode, targetNode)
      }
    }
    else if (m.isInstanceOf[SeqGroupOp] && t.isInstanceOf[SpecialOp]) {
      Regexify.orGroup(mainNode, targetNode)
    }
    else if (m.isInstanceOf[OrGroupOp] && t.isInstanceOf[OrGroupOp]) {
      val torType = t.asInstanceOf[OrGroupOp].optype
      val morType = m.asInstanceOf[OrGroupOp].optype
      if (morType.equals(Regexify.seq) && (torType.equals(Regexify.seq) || torType.equals(Regexify.or) || torType.equals(Regexify.oneplus) || torType.equals(Regexify.seqrepeat))) {
        Regexify.orGroup(mainNode, targetNode)
      }
      else if (morType.equals(Regexify.or) && torType.equals(Regexify.or)) {
        Regexify.orContentGroup(mainNode, targetNode)
      }
      else if (morType.equals(Regexify.anystar) || morType.equals(Regexify.anyplus) || morType.equals(Regexify.any)) {
        mainNode
      }
      else if (torType.equals(Regexify.anystar) || torType.equals(Regexify.anyplus) || torType.equals(Regexify.any)) {
        targetNode
      }
      else {
        null
      }
    }
    else {
      null
    }
  }*/
}