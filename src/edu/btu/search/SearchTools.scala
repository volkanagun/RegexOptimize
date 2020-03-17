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


//multiple path scoring over a global lattice structure
//multiple positive and negative path merge
case class Path(var cells: Seq[Cell] = Seq(), var cost: Double = 0d) extends Serializable {

  def addCell(cell: Cell, blockCost: Double): this.type = {
    cells :+= cell
    cost += blockCost
    this
  }

  def addCell(cell: Cell, blockCost: Double, zigzagCost: Double): this.type = {
    if (cells.isEmpty) {
      addCell(cell, blockCost)
    }
    else {
      if (getLastCell().directional(cell)) {
        addCell(cell, blockCost)
      }
      else {
        cells :+= cell
        cost = cost + blockCost + zigzagCost
      }

    }
    this
  }

  def copy(): Path = {
    Path(cells, cost)
  }

  def hasLastCell(): Boolean = {
    !cells.isEmpty
  }

  def getLastCell(): Cell = {
    cells.last
  }

  override def toString(): String = {
    cells.mkString("\n")
  }

  def toOrCell(matching: Int, i: Int, j: Int, sources: Seq[RegexNodeIndex], targets: Seq[RegexNodeIndex]): Cell = {

    val source = Regexify.direct(sources)
    val target = Regexify.direct(targets)

    Cell(i, j, source.toRegexNodeIndex(i), target.toRegexNodeIndex(j), matching, 1)

  }

  def addNode(sequence: Seq[RegexNodeIndex], thisNode: RegexNodeIndex): Seq[RegexNodeIndex] = {
    if (!sequence.contains(thisNode)) {
      val nsequence = sequence :+ thisNode
      nsequence
    }
    else {
      sequence
    }
  }


  def summarize(): this.type = {

    var sources = Seq[RegexNodeIndex]()
    var targets = Seq[RegexNodeIndex]()
    var newCells = Seq[Cell]()

    for (i <- 0 until cells.length) {
      //decide down or right
      val crrCell = cells(i)
      val (crri, crrj) = (crrCell.i, crrCell.j)

      if (crrCell.matching == 1 && crrCell.isCross()) {
        sources = addNode(sources, crrCell.source)
        targets = addNode(targets, crrCell.target)

      }
      else if (crrCell.isDown()) {
        sources = addNode(sources, crrCell.source)
        targets = addNode(targets, crrCell.target)

      }
      else if (crrCell.isRight()) {
        sources = addNode(sources, crrCell.source)
        targets = addNode(targets, crrCell.target)

      }
      else if (!sources.isEmpty && !targets.isEmpty) {
        //or matching
        if (sources.last.equals(crrCell.source)) sources = sources.take(sources.length - 1)
        if (targets.last.equals(crrCell.target)) targets = targets.take(targets.length - 1)

        newCells = newCells :+ toOrCell(1, 1, 1, sources, targets)
        newCells :+= crrCell
        sources = Seq()
        targets = Seq()
      }
      else {
        newCells :+= crrCell
      }
    }

    if (!sources.isEmpty || !targets.isEmpty) {
      newCells = newCells :+ toOrCell(1, 1, 1, sources, targets)
    }

    cells = newCells
    this
  }

  def toOrRegex(): RegexParam = {
    toOrRegex(cells, new RegexParam())
  }

  def toOrRegex(cells: Seq[Cell], newParam: RegexParam): RegexParam = {

    val crrCell = cells.head
    val nextCells = cells.tail
    val prevCell = newParam.prevCell

    //need order between appends for reconstruction
    if ( /*crrCell.isCross() && */ crrCell.isEqualWithoutIndex()) {
      newParam.addRegex(crrCell.largestIndice())
      newParam.sourceRemove(crrCell.source)
      newParam.targetRemove(crrCell.target)
      newParam.mainRegex += newParam.constructRegex()
    }
      else if (prevCell == null) {
      newParam.sourceTop(crrCell.source)
      newParam.targetTop(crrCell.target)
    }
    else if (prevCell.directional(crrCell) && crrCell.isDown()) {
      newParam.sourceTop(crrCell.source)
    }
    else if (prevCell.directional(crrCell) && crrCell.isRight()) {
      newParam.targetTop(crrCell.target)
    }
    else if (crrCell.isCross()) {
      newParam.sourceTop(crrCell.source)
      newParam.targetTop(crrCell.target)
    }

    if (!nextCells.isEmpty) {
      toOrRegex(nextCells, newParam.setPrevCell(crrCell))
    }
    else newParam

  }


  def toRegex(cells: Seq[Cell], prevParam: RegexParam): Seq[String] = {

    if (cells.isEmpty) {

      val newOptional = if (prevParam.prevCell.matching > 1 && !prevParam.optional.isEmpty) prevParam.optional + "?)" else if (!prevParam.optional.isEmpty) prevParam.optional + "?)" else ""
      var newRegex = if (prevParam.count > 0) prevParam.regex + prevParam.prevCell.toRegex(1, prevParam.count) else prevParam.regex

      newRegex = if (!prevParam.sourceContinue.isEmpty && !prevParam.targetContinue.isEmpty) newRegex + s"(${prevParam.sourceContinue} | ${prevParam.targetContinue})" else newRegex

      val finalRegex = if (prevParam.or.isEmpty && newOptional.isEmpty) newRegex
      else if (prevParam.or.isEmpty) newRegex + newOptional
      else if (newOptional.isEmpty) newRegex + "(" + prevParam.or + ")"
      else newRegex + "(" + prevParam.or + ")" + newOptional

      Seq(finalRegex)
    }
    else if (prevParam.prevCell == null) {

      val crrCell = cells.head

      val newParam = new RegexParam()
        .setPrevCell(crrCell)
        .setOr(prevParam.or)
        .setRegex(prevParam.regex)
        .setOptional(prevParam.optional)

      if (crrCell.matching == 0) {

        val finalRegex = prevParam.regex + toRegex(cells.tail, newParam)
        Seq(finalRegex)

      }
      else if (crrCell.matching == 1) {

        val newOr = prevParam.or + "|" + Regexify.toOrRegex(newParam.prevCell.source, newParam.prevCell.target)
        val newSource = prevParam.sourceContinue + newParam.prevCell.source.matchGroup
        val newTarget = prevParam.targetContinue + newParam.prevCell.target.matchGroup

        newParam.setOr(newOr)
          .setSource(newSource)
          .setTarget(newTarget)

        toRegex(cells.tail, newParam)
      }
      else if (crrCell.matching == 2) {
        val newOptional = prevParam.optional + "(" + Regexify.toOrRegex(newParam.prevCell.source, newParam.prevCell.target)
        newParam.setOptional(newOptional)
        toRegex(cells.tail, newParam)
      }
      else if (crrCell.matching == 3) {
        val newOptional = prevParam.optional + newParam.prevCell.source.matchGroup
        newParam.setOptional(newOptional)
        toRegex(cells.tail, newParam)
      }
      else if (crrCell.matching == 4) {
        val newOptional = prevParam.optional + newParam.prevCell.target.matchGroup
        newParam.setOptional(newOptional)
        toRegex(cells.tail, newParam)
      }
      else {
        null
      }


    }
    else {

      val newParam = new RegexParam()
        .setPrevCell(cells.head)
        .setOr(prevParam.or)
        .setRegex(prevParam.regex)
        .setOptional(prevParam.optional)

      //for non-grouping
      if (prevParam.prevCell.matching == 0 && newParam.prevCell.matching == 0) {
        newParam.setCount(prevParam.count + 1)
        toRegex(cells.tail, newParam)
      }
      else if (prevParam.prevCell.matching == 0 && newParam.prevCell.matching == 1) {
        val newOr = Regexify.toOrRegex(newParam.prevCell.source, newParam.prevCell.target)
        newParam.setOr(newOr).setCount(0)
        toRegex(cells.tail, prevParam)
      }
      else if (prevParam.prevCell.matching == 0 && newParam.prevCell.matching == 2) {
        val newOptional = prevParam.optional + "(" + Regexify.toOrRegex(newParam.prevCell.source, newParam.prevCell.target)
        newParam.setOptional(newOptional)
        toRegex(cells.tail, newParam)
      }
      else if (prevParam.prevCell.matching == 1 && newParam.prevCell.matching == 0) {
        val newRegex = prevParam.regex + (if (!prevParam.or.isEmpty) "(" + prevParam.or + ")" + newParam.prevCell.toRegex() else newParam.prevCell.toRegex())
        newParam.setRegex(newRegex)
        toRegex(cells.tail, newParam)
      }
      else if (prevParam.prevCell.matching == 1 && newParam.prevCell.matching == 1) {

        val newOr = prevParam.or + "|" + Regexify.toOrRegex(newParam.prevCell.source, newParam.prevCell.target)
        val newSource = prevParam.sourceContinue + newParam.prevCell.source.matchGroup
        val newTarget = prevParam.targetContinue + newParam.prevCell.target.matchGroup

        newParam.setOr(newOr)
          .setSource(newSource)
          .setTarget(newTarget)

        toRegex(cells.tail, newParam)
      }
      else if (prevParam.prevCell.matching == 1 && newParam.prevCell.matching == 2) {

        val newOptional = prevParam.optional + "(" + Regexify.toOrRegex(newParam.prevCell.source, newParam.prevCell.target)
        val newRegex = prevParam.regex + "(" + prevParam.or + ")"
        newParam.setRegex(newRegex).setOptional(newOptional)
        toRegex(cells.tail, newParam)

      }
      else if (prevParam.prevCell.matching == 2 && newParam.prevCell.matching == 0) {
        //use optional in target
        //flash or and optional to regex
        val newOr = if (prevParam.or.isEmpty) prevParam.or else "(" + prevParam.or + ")"
        val newOptional = if (prevParam.optional.isEmpty) prevParam.optional else prevParam.optional + "?)"
        val newRegex = prevParam.regex + newOr + newOptional + newParam.prevCell.toRegex()

        newParam.setCount(0).setRegex(newRegex)
          .setOptional(newOptional)
          .setOr(newOr)

        toRegex(cells.tail, newParam)
      }
      else if (prevParam.prevCell.matching == 2 && newParam.prevCell.matching == 1) {
        //use optional in target

        val newOr = newParam.prevCell.source.matchValue + "|" + newParam.prevCell.target.matchValue

        newParam.sourceContinue += newParam.prevCell.source.matchValue
        newParam.targetContinue += newParam.prevCell.target.matchValue

        newParam.setOr(newOr)
        toRegex(cells.tail, newParam)

      }
      else if (prevParam.prevCell.matching == 2 && newParam.prevCell.matching == 2) {

        val newOptional = prevParam.optional + "(" + Regexify.toOrRegex(newParam.prevCell.source, newParam.prevCell.target)
        newParam.setOptional(newOptional)
        toRegex(cells.tail, newParam)

      }
      else if (prevParam.prevCell.matching != 3 && newParam.prevCell.matching == 3) {

        val newOptional = if (!prevParam.optional.isEmpty) prevParam.optional + ")(" + Regexify.toOrRegex(newParam.prevCell.source, newParam.prevCell.target)
        else "(" + Regexify.toOrRegex(newParam.prevCell.source, newParam.prevCell.target)

        newParam.setOptional(newOptional)
        toRegex(cells.tail, newParam)

      }
      else if (prevParam.prevCell.matching == 3 && newParam.prevCell.matching == 3) {

        val newOptional = prevParam.optional + newParam.prevCell.source.matchValue
        newParam.setOptional(newOptional)
        toRegex(cells.tail, newParam)

      }
      else if (prevParam.prevCell.matching != 4 && newParam.prevCell.matching == 4) {

        val newOptional = if (!prevParam.optional.isEmpty) prevParam.optional + ")(" + Regexify.toOrRegex(newParam.prevCell.source, newParam.prevCell.target)
        else "(" + Regexify.toOrRegex(newParam.prevCell.source, newParam.prevCell.target)
        newParam.setOptional(newOptional)
        toRegex(cells.tail, newParam)

      }
      else if (prevParam.prevCell.matching == 4 && newParam.prevCell.matching == 4) {

        val newOptional = prevParam.optional + newParam.prevCell.target.matchValue
        newParam.setOptional(newOptional)
        toRegex(cells.tail, newParam)

      }
      else Seq(prevParam.regex)
    }
  }

  def toExactRegex(cells: Seq[Cell], prevParam: RegexParam): Seq[String] = {

    if (cells.isEmpty) {
      val newOptional = if (prevParam.prevCell.matching > 1 && !prevParam.optional.isEmpty) prevParam.optional + "?)" else if (!prevParam.optional.isEmpty) prevParam.optional + "?)" else ""
      val newRegex = if (prevParam.count > 0) prevParam.regex + prevParam.prevCell.toExactRegex(1, prevParam.count) else prevParam.regex + prevParam.prevCell.toExactRegex()

      val finalRegex = if (prevParam.sourceContinue.length > 1 && prevParam.targetContinue.length > 1) newRegex
      else if (prevParam.or.isEmpty && newOptional.isEmpty) newRegex
      else if (prevParam.or.isEmpty) newRegex + newOptional
      else if (newOptional.isEmpty && !prevParam.or.isEmpty) newRegex + "(" + prevParam.or + ")"
      else if (!prevParam.or.isEmpty) newRegex + "(" + prevParam.or + ")" + newOptional
      else newRegex

      Seq(finalRegex)
    }
    else if (prevParam.prevCell == null) {

      val crrCell = cells.head
      val newParam = new RegexParam()
        .setPrevCell(crrCell)
        .setOr(prevParam.or)
        .setRegex(prevParam.regex)
        .setOptional(prevParam.optional)

      if (crrCell.matching == 0) {

        val finalRegex = prevParam.regex + toRegex(cells.tail, newParam)
        Seq(finalRegex)

      }
      else if (crrCell.matching == 1) {

        val newOr = prevParam.or + "|" + Regexify.toOrExactRegex(newParam.prevCell.source, newParam.prevCell.target)
        val newSource = prevParam.sourceContinue + newParam.prevCell.source.matchGroup
        val newTarget = prevParam.targetContinue + newParam.prevCell.target.matchGroup

        newParam.setOr(newOr)
          .setSource(newSource)
          .setTarget(newTarget)

        toRegex(cells.tail, newParam)

      }
      else if (crrCell.matching == 2) {
        val newOptional = prevParam.optional + "(" + Regexify.toOrExactRegex(newParam.prevCell.source, newParam.prevCell.target)
        newParam.setOptional(newOptional)
        toRegex(cells.tail, newParam)
      }
      else if (crrCell.matching == 3) {
        val newOptional = prevParam.optional + newParam.prevCell.source.matchGroup
        newParam.setOptional(newOptional)
        toRegex(cells.tail, newParam)
      }
      else if (crrCell.matching == 4) {
        val newOptional = prevParam.optional + newParam.prevCell.target.matchGroup
        newParam.setOptional(newOptional)
        toRegex(cells.tail, newParam)
      }
      else {
        null
      }


    }
    else {

      val newParam = new RegexParam()
        .setPrevCell(cells.head)
        .setOr(prevParam.or)
        .setRegex(prevParam.regex)
        .setSource(prevParam.sourceContinue)
        .setTarget(prevParam.targetContinue)
        .setOptional(prevParam.optional)

      //Equal Matching
      if (prevParam.prevCell.matching == 0 && newParam.prevCell.matching == 0) {
        newParam.setCount(prevParam.count + 1)
        toExactRegex(cells.tail, newParam)
      }
      else if (prevParam.prevCell.matching == 0 && newParam.prevCell.matching == 1) {

        val newOr = "(" + Regexify.toOrExactRegex(newParam.prevCell.source, newParam.prevCell.target) + ")"
        val newSource = newParam.prevCell.source.matchValue
        val newTarget = newParam.prevCell.target.matchValue
        val newRegex = prevParam.regex + prevParam.prevCell.source.matchValue
        newParam.setOr(newOr).setCount(0)
          .setSource(newSource).setRegex(newRegex)
          .setTarget(newTarget)

        toExactRegex(cells.tail, newParam)

      }
      else if (prevParam.prevCell.matching == 0 && newParam.prevCell.matching == 2) {
        val newOptional = prevParam.optional + "(" + Regexify.toOrExactRegex(newParam.prevCell.source, newParam.prevCell.target)
        newParam.setOptional(newOptional)
        toExactRegex(cells.tail, newParam)
      }
      else if (prevParam.prevCell.matching == 1 && newParam.prevCell.matching == 0) {
        var newRegex = if (prevParam.sourceContinue.length <= 1 || prevParam.targetContinue.length <= 1) {
          val returnRegex = prevParam.regex + (if (!prevParam.or.isEmpty) "(" + prevParam.or + ")" + newParam.prevCell.toExactRegex() else newParam.prevCell.toExactRegex())
          newParam.setOr("")
          returnRegex
        }
        else {
          val returnRegex = prevParam.regex + "(" + Regexify.toOrExactRegex(newParam.prevCell.source, newParam.prevCell.target) + ")"
          returnRegex
        }

        newParam.setRegex(newRegex)
        toExactRegex(cells.tail, newParam)
      }
      else if (prevParam.prevCell.matching == 1 && newParam.prevCell.matching == 1) {

        val newOr = prevParam.or + "(" + Regexify.toOrExactRegex(newParam.prevCell.source, newParam.prevCell.target) + ")"
        val newSource = prevParam.sourceContinue + newParam.prevCell.source.matchValue
        val newTarget = prevParam.targetContinue + newParam.prevCell.target.matchValue

        newParam.setOr(newOr)
          .setSource(newSource)
          .setTarget(newTarget)

        toExactRegex(cells.tail, newParam)
      }
      else if (prevParam.prevCell.matching == 1 && newParam.prevCell.matching == 2) {

        val newOptional = prevParam.optional + "(" + Regexify.toOrExactRegex(newParam.prevCell.source, newParam.prevCell.target)
        val newRegex = prevParam.regex + "(" + prevParam.or + ")"
        newParam.setRegex(newRegex).setOptional(newOptional)
        toExactRegex(cells.tail, newParam)

      }
      else if (prevParam.prevCell.matching == 2 && newParam.prevCell.matching == 0) {
        //use optional in target
        //flash or and optional to regex
        val newOr = if (prevParam.or.isEmpty) prevParam.or else "(" + prevParam.or + ")"
        val newOptional = if (prevParam.optional.isEmpty) prevParam.optional else prevParam.optional + "?)"
        val newRegex = prevParam.regex + newOr + newOptional + newParam.prevCell.toRegex()

        newParam.setCount(0).setRegex(newRegex)
          .setOptional(newOptional)
          .setOr(newOr)

        toExactRegex(cells.tail, newParam)
      }
      else if (prevParam.prevCell.matching == 2 && newParam.prevCell.matching == 1) {
        //use optional in target
        val newOr = Regexify.toOrExactRegex(newParam.prevCell.source, newParam.prevCell.target)
        newParam.setOr(newOr)
        toExactRegex(cells.tail, newParam)

      }
      else if (prevParam.prevCell.matching == 2 && newParam.prevCell.matching == 2) {

        val newOptional = prevParam.optional + "(" + Regexify.toOrExactRegex(newParam.prevCell.source, newParam.prevCell.target)
        newParam.setOptional(newOptional)
        toExactRegex(cells.tail, newParam)

      }
      else if (prevParam.prevCell.matching != 3 && newParam.prevCell.matching == 3) {

        val newOptional = if (!prevParam.optional.isEmpty) prevParam.optional + ")(" + Regexify.toOrExactRegex(newParam.prevCell.source, newParam.prevCell.target)
        else "(" + Regexify.toOrExactRegex(newParam.prevCell.source, newParam.prevCell.target)

        newParam.setOptional(newOptional)
        toExactRegex(cells.tail, newParam)

      }
      else if (prevParam.prevCell.matching == 3 && newParam.prevCell.matching == 3) {

        val newOptional = prevParam.optional + newParam.prevCell.source.matchValue
        newParam.setOptional(newOptional)
        toExactRegex(cells.tail, newParam)

      }
      else if (prevParam.prevCell.matching != 4 && newParam.prevCell.matching == 4) {

        val newOptional = if (!prevParam.optional.isEmpty) prevParam.optional + ")(" + Regexify.toOrExactRegex(newParam.prevCell.source, newParam.prevCell.target)
        else "(" + Regexify.toOrExactRegex(newParam.prevCell.source, newParam.prevCell.target)
        newParam.setOptional(newOptional)
        toExactRegex(cells.tail, newParam)

      }
      else if (prevParam.prevCell.matching == 4 && newParam.prevCell.matching == 4) {
        val newOptional = prevParam.optional + newParam.prevCell.target.matchValue
        newParam.setOptional(newOptional)
        toExactRegex(cells.tail, newParam)
      }
      else Seq(prevParam.regex)
    }
  }

  def toRegex(): Seq[String] = {
    toRegex(cells, new RegexParam())
  }

  def toExactRegex(): Seq[String] = {
    toExactRegex(cells, new RegexParam())
  }


}

object SearchTest {


}