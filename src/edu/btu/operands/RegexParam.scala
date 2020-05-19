package edu.btu.operands


import scala.collection.immutable.Stack
import scala.util.Random


class RegexParam extends Serializable {

  var regex: String = ""
  var optional: String = ""
  var or: String = ""
  var sourceContinue: String = ""
  var targetContinue: String = ""
  var count: Int = 0
  var mainRegex = ""

  var sourceOptional = Stack[RegexNodeIndex]()
  var targetOptional = Stack[RegexNodeIndex]()
  var regexStack = Stack[RegexNodeIndex]()

  var prevCell: Cell = null


  private def stackTop(optionalStack: Stack[RegexNodeIndex], regexNode: RegexNodeIndex): (Stack[RegexNodeIndex], RegexNodeIndex) = {
    if (optionalStack.isEmpty) {
      (optionalStack.push(regexNode), regexNode)
    }
    else if (optionalStack.top.canEqual(regexNode)) {
      (optionalStack, regexNode)
    }
    else {
      (optionalStack.push(regexNode), regexNode)
    }
  }

  def sourceTop(regexNode: RegexNodeIndex): RegexNodeIndex = {
    val (ss, newNode) = stackTop(sourceOptional, regexNode)
    sourceOptional = ss
    newNode
  }

  def targetTop(regexNode: RegexNodeIndex): RegexNodeIndex = {
    val (ss, newNode) = stackTop(targetOptional, regexNode)
    targetOptional = ss
    newNode
  }

  def regexTop(regexNode: RegexNodeIndex): RegexNodeIndex = {
    val (ss, newNode) = stackTop(regexStack, regexNode)
    regexStack = ss
    newNode
  }

  def addRegex(regexNode: RegexNodeIndex): this.type = {
    if(regexStack.isEmpty || !regexStack.top.equals(regexNode)) {
      regexStack = regexStack.push(regexNode)
    }

    this
  }

  def addRegex(cell: Cell) : this.type = {

    if (!regexStack.isEmpty && regexStack.top.equals(cell.source)) {
      regexStack = regexStack.push(cell.target)
    }
    else if (!regexStack.isEmpty && regexStack.top.equals(cell.target)) {
      regexStack = regexStack.push(cell.target)
    }
    else {
      regexStack = regexStack.push(cell.largestIndice())
    }

    this
  }

  def removeTopEqual(optionalStack: Stack[RegexNodeIndex], regexNode: RegexNodeIndex): Stack[RegexNodeIndex] = {
    if (!optionalStack.isEmpty && optionalStack.top.canMatch(regexNode)) {
      optionalStack.pop
    }
    else {
      optionalStack
    }
  }

  def largestIndice(pair: (RegexNodeIndex, RegexNodeIndex)): RegexNodeIndex = {
    if (pair._1.indice >= pair._2.indice) pair._1
    else pair._2
  }

  def constructRegexNode(): RegexNodeIndex = {

    val mainNode = RegexNodeIndex(0, RegexOp("seq"), regexStack.reverse)
    regexStack = Stack()

    var regexSrcOpt = Seq[RegexNodeIndex]()
    var regexTrtOpt = Seq[RegexNodeIndex]()

    var srcOpt: RegexNodeIndex = null;
    var trtOpt: RegexNodeIndex = null;

    while (!sourceOptional.isEmpty || !targetOptional.isEmpty) {

      if (!sourceOptional.isEmpty && !targetOptional.isEmpty) {
        val s = sourceOptional.pop2
        srcOpt = s._1
        sourceOptional = s._2

        val t = targetOptional.pop2
        trtOpt = t._1
        targetOptional = t._2

        regexSrcOpt = srcOpt +: regexSrcOpt
        regexTrtOpt = trtOpt +: regexTrtOpt
      }
      else if (!sourceOptional.isEmpty) {
        val s = sourceOptional.pop2
        srcOpt = s._1;
        sourceOptional = s._2
        regexSrcOpt = srcOpt +: regexSrcOpt
      }
      else if (!targetOptional.isEmpty) {
        val t = targetOptional.pop2
        trtOpt = t._1;
        targetOptional = t._2
        regexTrtOpt = trtOpt +: regexTrtOpt
      }
    }

    if (regexSrcOpt.isEmpty && !regexTrtOpt.isEmpty) {
      mainNode.elems = RegexNodeIndex(0, RegexOp(Regexify.optional), regexTrtOpt) +: mainNode.elems
    }
    else if (regexTrtOpt.isEmpty && !regexSrcOpt.isEmpty) {
      mainNode.elems = RegexNodeIndex(0, RegexOp(Regexify.optional), regexSrcOpt) +: mainNode.elems
    }

    else {
      val srcNode = RegexNodeIndex(0, RegexOp(Regexify.seq), regexSrcOpt)
      val dstNode = RegexNodeIndex(0, RegexOp(Regexify.seq), regexTrtOpt)
      mainNode.elems = RegexNodeIndex(0, RegexOp(Regexify.or), Seq(srcNode, dstNode)) +: mainNode.elems
    }

    mainNode.simplify()

  }


  def constructRegex(): String = {

    var regexStr = regexStack.reverse.map(node => node.toRegex()).mkString("")
    regexStack = Stack()

    var regexSrcOpt = ""
    var regexTrtOpt = ""

    var srcOpt: RegexNodeIndex = null;
    var trtOpt: RegexNodeIndex = null;


    while (!sourceOptional.isEmpty || !targetOptional.isEmpty) {


      if (!sourceOptional.isEmpty && !targetOptional.isEmpty) {
        val s = sourceOptional.pop2
        srcOpt = s._1
        sourceOptional = s._2

        val t = targetOptional.pop2
        trtOpt = t._1
        targetOptional = t._2

        regexSrcOpt = srcOpt.matchValue + regexSrcOpt
        regexTrtOpt = trtOpt.matchValue + regexTrtOpt

      }
      else if (!sourceOptional.isEmpty) {

        val s = sourceOptional.pop2
        srcOpt = s._1;
        sourceOptional = s._2

        regexSrcOpt = srcOpt.matchValue + regexSrcOpt

      }
      else if (!targetOptional.isEmpty) {
        val t = targetOptional.pop2
        trtOpt = t._1;
        targetOptional = t._2

        regexTrtOpt = trtOpt.matchValue + regexTrtOpt
      }


    }

    (if (regexTrtOpt.isEmpty && regexSrcOpt.isEmpty) regexStr
    else if (regexSrcOpt.isEmpty) "(" + regexTrtOpt + "?)" + regexStr
    else if (regexTrtOpt.isEmpty) "(" + regexSrcOpt + "?)" + regexStr
    else if(regexStr.isEmpty) "(" + regexSrcOpt + "|" + regexTrtOpt + ")"
    else "(" + regexSrcOpt + "|" + regexTrtOpt + "?)" + regexStr)

  }

  def randomRegex(seed:Int): String = {
    val rnd = new Random(seed)
    var regexStr = regexStack.reverse.map(node => node.getRndValue(rnd)).mkString("")

    regexStack = Stack()

    var regexSrcOpt = ""
    var regexTrtOpt = ""

    var srcOpt: RegexNodeIndex = null;
    var trtOpt: RegexNodeIndex = null;


    while (!sourceOptional.isEmpty || !targetOptional.isEmpty) {


      if (!sourceOptional.isEmpty && !targetOptional.isEmpty) {
        val s = sourceOptional.pop2
        srcOpt = s._1
        sourceOptional = s._2

        val t = targetOptional.pop2
        trtOpt = t._1
        targetOptional = t._2

        regexSrcOpt = srcOpt.getRndValue(rnd) + regexSrcOpt
        regexTrtOpt = trtOpt.getRndValue(rnd) + regexTrtOpt

      }
      else if (!sourceOptional.isEmpty) {

        val s = sourceOptional.pop2
        srcOpt = s._1;
        sourceOptional = s._2

        regexSrcOpt = srcOpt.getRndValue(rnd) + regexSrcOpt

      }
      else if (!targetOptional.isEmpty) {
        val t = targetOptional.pop2
        trtOpt = t._1;
        targetOptional = t._2

        regexTrtOpt = trtOpt.getRndValue(rnd) + regexTrtOpt
      }


    }

    (if (regexTrtOpt.isEmpty && regexSrcOpt.isEmpty) regexStr
    else if (regexSrcOpt.isEmpty) "(" + regexTrtOpt + "?)" + regexStr
    else if (regexTrtOpt.isEmpty) "(" + regexSrcOpt + "?)" + regexStr
    else if(regexStr.isEmpty) "(" + regexSrcOpt + "|" + regexTrtOpt + ")"
    else "(" + regexSrcOpt + "|" + regexTrtOpt + "?)" + regexStr)

  }

  def updateRegex(): String = {
    mainRegex = mainRegex + constructRegex()
    mainRegex
  }

  def randomRegex(): Seq[String] = {
    var seeds = Seq(17, 29, 37, 1017)
    seeds.map(seed=> mainRegex + randomRegex(seed))

  }

  def sourceTop(): RegexNodeIndex = {
    if (sourceOptional.isEmpty) null
    else sourceOptional.top
  }

  def sourceRemove(regexNode: RegexNodeIndex): this.type = {
    sourceOptional = removeTopEqual(sourceOptional, regexNode)
    this
  }

  def targetRemove(regexNode: RegexNodeIndex): this.type = {
    targetOptional = removeTopEqual(targetOptional, regexNode)
    this
  }

  def regexRemove(regexNode: RegexNodeIndex): this.type = {
    regexStack = removeTopEqual(regexStack, regexNode)
    this
  }


  def targetTop(): RegexNodeIndex = {
    if (targetOptional.isEmpty) null
    else targetOptional.top
  }


  def setPrevCell(cell: Cell): this.type = {
    this.prevCell = cell
    this
  }

  def setRegex(regex: String): this.type = {
    this.regex = regex
    this
  }

  def setOptional(regex: String): this.type = {
    this.optional = regex
    this
  }

  def setOr(regex: String): this.type = {
    this.or = regex
    this
  }

  def setSource(source: String): this.type = {
    this.sourceContinue = source
    this
  }

  def setTarget(target: String): this.type = {
    this.targetContinue = target
    this
  }

  def setCount(count: Int): this.type = {
    this.count = count
    this
  }

}
