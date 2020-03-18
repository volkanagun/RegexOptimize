package edu.btu.search

import edu.btu.operands.{RegexNode, RegexNodeIndex, RegexOp, Regexify}

import scala.collection.immutable.Stack


case class Matrix(source: Seq[RegexNodeIndex], target: Seq[RegexNodeIndex], cellContents: Array[Array[CellContent]]) extends Serializable {

  def getCellContent(i: Int, j: Int): CellContent = {
    cellContents(i)(j)
  }

  def getCells(i: Int, j: Int): Seq[Cell] = {
    getCellContent(i, j).cells
  }


  def getColSize(): Int = {
    if (cellContents.isEmpty) 0
    else cellContents.head.length
  }

  def getRowSize(): Int = {
    if (cellContents.isEmpty) 0
    else cellContents.length
  }

  def getRowFrom(i: Int, from: Int): Array[CellContent] = {
    if (from < getColSize()) cellContents(i).slice(from, getColSize())
    else Array()
  }

  /**
   * Merhaba arkadaşlar,
   *
   * Ödevi github üzerine göndereceksiniz. Github için kullandığınız email adresini
   *
   *
   *
   * Konu: NYP Github
   *
   * İçerik: email@usr.com
   *
   *
   *
   * yukarıdaki şekilde ahmet.kasif@btu.edu.tr
   */


}

case class CellContent(i: Int, j: Int, var cells: Seq[Cell] = Seq()) extends Serializable {
  def addCell(cell: Cell): this.type = {
    cells :+= cell
    this
  }


}

case class Cell(i: Int, j: Int, var source: RegexNodeIndex = null, var target: RegexNodeIndex = null, var matching: Int = -1, var cost: Double = -1d) {

  var rowEnd = false;
  var colEnd = false;
  var isNegative = false;


  def setNegative():this.type ={
    isNegative = true;
    this
  }

  def setRowEnd(): this.type = {
    rowEnd = true
    this
  }

  def setColEnd(): this.type = {
    colEnd = true
    this
  }

  def largestIndice():RegexNodeIndex={
    if(source.indice >= target.indice) source
    else target
  }

  def isCross(): Boolean = {
    i == j
  }


  def isDown(): Boolean = {
    i >= j + 1
  }


  def isRight(): Boolean = {
    i + 1 <= j
  }

  def isLast(rowLength: Int, columnLength: Int): Boolean = {
    i == (rowLength - 1) && j == (columnLength - 1)
  }

  def isEqual(): Boolean = {
    source.canEqual(target)
  }

  def isEqualWithoutIndex(): Boolean = {
    source.canEqualMain(target)
  }


  def directional(cell: Cell): Boolean = {
    (isCross() && cell.isCross()) || (isDown() && cell.isDown()) || (isRight() && cell.isRight()) || (isCross() && (cell.isRight() || cell.isDown()))
  }

  def negativeAll(cell:Cell):Boolean={
    val array = Array(source.negative(cell.source),  target.negative(cell.target), source.negative(cell.target), target.negative(cell.source))
    array.forall(i => i)
  }

  def negativeAny(cell:Cell):Boolean={
    val array = Array(source.negative(cell.source),  target.negative(cell.target), source.negative(cell.target), target.negative(cell.source))
    array.exists(i=>i)
  }

  def negativeSingle(cell:Cell):Boolean={
    source.negative(cell.source) || target.negative(cell.target) || source.negative(cell.target) || target.negative(cell.source)
  }


  override def canEqual(that: Any): Boolean = that.isInstanceOf[Cell]

  override def equals(obj: Any): Boolean = {

    if (canEqual(obj)) {
      val ins = obj.asInstanceOf[Cell]
      ins.i == i && ins.j == j
    }
    else {
      false
    }
  }

  override def toString(): String = {
    s"Source: ${source}\nTarget: ${target}"
  }


  def toRegex(): String = {

    if (matching == 0 && (source.elems.length > 1 || target.elems.length > 1)) {
      val min = math.min(source.elems.length, target.elems.length)
      val max = math.max(source.elems.length, target.elems.length)
      if (min == max) source.matchGroup + "{" + min + "}"
      else if (max > min) source.matchGroup + "{" + min + "," + max + "}"
      else source.matchGroup + "+"
    }
    else if (matching == 0) {
      source.matchGroup
    }
    else if (matching == 1) source.matchGroup + "|" + target.matchGroup
    else if (matching == 2) source.matchGroup + "(" + target.matchGroup + "?)"
    else ""

  }

  def toExactRegex(): String = {

    if (matching == 0 && (source.elems.length > 1 || target.elems.length > 1)) {
      val min = math.min(source.elems.length, target.elems.length)
      val max = math.max(source.elems.length, target.elems.length)
      if (min == max) source.matchGroup + "{" + min + "}"
      else if (max > min) source.matchGroup + "{" + min + "," + max + "}"
      else source.matchValue + "+"
    }
    else if (matching == 0) {
      source.matchValue
    }
    else if (matching == 1) source.matchValue + "|" + target.matchValue
    else if (matching == 2) source.matchValue + "(" + target.matchValue + "?)"
    else ""

  }

  def toRegex(mmin: Int, mmax: Int): String = {
    if (matching == 0) {
      val min = math.min(math.min(source.elems.length, target.elems.length), mmin)
      val max = math.max(math.max(source.elems.length, target.elems.length), mmax)
      if (min == max) source.matchGroup + "{" + min + "}"
      else if (max > min) source.matchGroup + "{" + min + "," + max + "}"
      else source.matchGroup + "+"
    }
    else if (matching == 0) {
      source.matchGroup
    }
    else if (matching == 1) source.matchGroup + "|" + target.matchGroup
    else if (matching == 2) source.matchGroup + "(" + target.matchGroup + "?)"
    else ""
  }


  def toExactRegex(mmin: Int, mmax: Int): String = {

    if (matching == 0) {
      val min = math.min(math.min(source.elems.length, target.elems.length), mmin)
      val max = math.max(math.max(source.elems.length, target.elems.length), mmax)
      if (min == max) source.matchGroup + "{" + min + "}"
      else if (max > min) source.matchGroup + "{" + min + "," + max + "}"
      else source.matchGroup + "+"
    }
    else if (matching == 0) {
      source.matchGroup
    }
    else if (matching == 1) source.matchGroup + "|" + target.matchValue
    else if (matching == 2) source.matchGroup + "(" + target.matchValue + "?)"
    else ""

  }


  def equalsBySourceGroup(cell: Cell): Boolean = {
    source.equalsByGroup(cell.source)
  }

  def equalsByTargetGroup(cell: Cell): Boolean = {
    target.equalsByGroup(cell.target)
  }


  def nextCells(matrix: Matrix): Seq[Cell] = {
    val sizex = matrix.cellContents.length
    val sizey = matrix.cellContents.head.length
    var nexts = Seq[Cell]()

    if (cost != 0 && i + 1 < sizex) {
      //down
      nexts ++= matrix.getCells(i + 1, j)
    }

    if (cost != 0 && j + 1 < sizey) {
      //right
      nexts ++= matrix.getCells(i, j + 1)
    }

    if (i + 1 < sizex && j + 1 < sizey) {
      //cross
      nexts ++= matrix.getCells(i + 1, j + 1)
    }
    else if (i + 1 < sizex) {
      nexts ++= matrix.getCells(i + 1, j)
    }
    else if (j + 1 < sizey) {
      nexts ++= matrix.getCells(i, j + 1)
    }

    nexts.sortBy(_.cost)
  }


  def nextMinCells(matrix: Matrix): Seq[Cell] = {
    val sizex = matrix.cellContents.length
    val sizey = matrix.cellContents.head.length
    var nexts = Seq[Cell]()

    if (cost != 0 && i + 1 < sizex) {
      //down
      nexts ++= matrix.getCells(i + 1, j)
    }

    if (cost != 0 && j + 1 < sizey) {
      //right
      nexts ++= matrix.getCells(i, j + 1)
    }

    if (i + 1 < sizex && j + 1 < sizey) {
      //cross
      nexts ++= matrix.getCells(i + 1, j + 1)
    }
    else if (i + 1 < sizex) {
      nexts ++= matrix.getCells(i + 1, j)
    }
    else if (j + 1 < sizey) {
      nexts ++= matrix.getCells(i, j + 1)
    }

    if (nexts.isEmpty) Seq()
    else {
      val sorted = nexts.sortBy(_.cost)
      val minCost = sorted.head.cost

      if (minCost == 0) Seq(sorted.head)
      else sorted
    }
  }
}

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
    else if (optionalStack.top.canMatch(regexNode)) {
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
    regexStack = regexStack.push(regexNode)
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

  def largestIndice(pair:(RegexNodeIndex, RegexNodeIndex)):RegexNodeIndex={
    if(pair._1.indice >= pair._2.indice) pair._1
    else pair._2
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

   (if(regexTrtOpt.isEmpty && regexSrcOpt.isEmpty) regexStr
   else if(regexSrcOpt.isEmpty) "("+regexTrtOpt +"?)" + regexStr
   else if(regexTrtOpt.isEmpty) "("+regexSrcOpt+"?)" + regexStr
   else "("+regexSrcOpt+"|"+regexTrtOpt+"?)"+regexStr)

  }

  def updateRegex():String = {
    mainRegex = mainRegex + constructRegex()
    mainRegex
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



object SearchTest {


}