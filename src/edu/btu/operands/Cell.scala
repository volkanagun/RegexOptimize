package edu.btu.operands

import edu.btu.search.Matrix


case class Cell(i: Int, j: Int, var source: RegexNodeIndex = null, var target: RegexNodeIndex = null, var matching: Int = -1, var cost: Double = -1d) {

  var rowEnd = false;
  var colEnd = false;
  var isNegative = false;

  def copy():Cell={
    val cell = Cell(i, j, source.copy(), target.copy(), matching, cost)
    cell.rowEnd = rowEnd
    cell.colEnd = colEnd
    cell.isNegative = isNegative
    cell
  }

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

  def negativeCombinations(cell:Cell):Array[Cell]={
    Array(Cell(source.indice,cell.source.indice, source,   cell.source),  Cell(source.indice, cell.target.indice, source, cell.target),
      Cell(target.indice, cell.source.indice, target, cell.source), Cell(target.indice, cell.target.indice, target, cell.target))
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

  def negativeSelf():Boolean={
    source.negative(target)
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
