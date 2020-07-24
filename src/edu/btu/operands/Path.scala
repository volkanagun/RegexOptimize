package edu.btu.operands

import scala.util.control.Breaks


case class Path(var cells: Seq[Cell] = Seq(), var cost: Double = 0d) extends Serializable {

  var negative = false
  var multiple = false


  def setMultiple(multiple: Boolean): this.type = {
    this.multiple = multiple
    this
  }

  def getMultiple(): Boolean = {
    multiple
  }

  def setNegative(negative: Boolean): this.type = {
    this.negative = negative
    this
  }

  def setConditionalNegative(negative: Boolean): this.type = {
    if (!this.negative) this.negative = negative
    this
  }


  def getNegative(): Boolean = {
    negative
  }

  def simplify(): this.type = {
    cells = cells.map(_.simplify())
    this
  }

  def searchCost(cell: Cell): Double = {
    val sourceNode = cell.source
    val targetNode = cell.target

    if (hasLastCell()) {
      val lastCell = getLastCell()

      if (lastCell.directional(cell) && sourceNode.equalsByValue(targetNode)) 0.0
      else if (lastCell.directional(cell) && sourceNode.equalsByGroup(targetNode)) 1.0
      else if (lastCell.directional(cell) && sourceNode.matchesByGroup(targetNode)) 2.0
      else if (lastCell.directional(cell)) 3.0
      else 4.0

    }
    else if (sourceNode.equalsByValue(targetNode)) 0.0
    else if (sourceNode.equalsByGroup(targetNode)) 1.0
    else if (sourceNode.matchesByGroup(targetNode)) 2.0
    else 3.0

  }

  def addCell(cell: Cell, blockCost: Double): this.type = {
    cells :+= cell.simplify()
    cost += blockCost
    this
  }

  def addCellUpdateCost(cell: Cell): this.type = {
    val blockCost = searchCost(cell)
    cells :+= cell.simplify()
    cost += blockCost
    this
  }

  def addCellUpdateCost(cells: Seq[Cell]): this.type = {
    cells.foreach(cell => addCellUpdateCost(cell))
    this
  }

  def addCell(addCells: Seq[Cell]): this.type = {
    cells ++= addCells.map(_.simplify())
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
        cells :+= cell.simplify()
        cost = cost + blockCost + zigzagCost
      }
    }

    this
  }


  def copy(): Path = {
    Path(cells, cost)
      .setNegative(negative)
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

  def contains(cell: Cell): Boolean = {
    cells.contains(cell)
  }

  def addCell(sequence: Set[Path], cell: Cell): Set[Path] = {
    sequence.map(crrPath => crrPath.copy().addCell(cell, cell.cost))
  }

  def addCell(sequence: Set[Path], cells: Set[Cell]): Set[Path] = {
    cells.flatMap(cell => addCell(sequence, cell))
  }

  def addCellBlock(sequence: Set[Path], cells: Seq[Cell]): Set[Path] = {
    sequence.map(path => path.addCellUpdateCost(cells))
  }

  def swap(): this.type = {
    this.cells = cells.map(cell => cell.swap())
    this
  }

  //rank by consuming order
  //separate


  //combine this path with a negative one
  //with minimum effort
  //algorithm??
  def negativePath(negative: Path): Set[Path] = {

    val minSize = math.min(negative.cells.length, cells.length)
    var newPath = Set[Path](Path())
    var i = 0
    var found = false

    while (i < minSize && !found) {

      val negCell = negative.cells(i).regexify()
      val crrCell = cells(i).copy().regexify()

      //create target negative cases
      //create negative nodes
      //lose path order
      //should create new paths instead for all negative combinations
      val negatedCells = crrCell.negativeCombinations(negCell)
        .map(cell => (cell, cell.negateTarget()))
        .filter(_._2).map(_._1).toSet

      if (negatedCells.isEmpty) {
        newPath = addCell(newPath, crrCell)
      }
      else {

        //negative path
        //for each negated cell create a new path
        newPath = addCell(newPath, crrCell)
        newPath = addCell(newPath, negatedCells)
        found = true
      }

      i += 1

    }

    if (i < cells.length)
      addCellBlock(newPath, cells.slice(i, cells.length)).map(_.setConditionalNegative(found)) //.negativeSummarization())
    else
      newPath.map(_.setConditionalNegative(found)) //.map(_.negativeSummarization())
  }


  //negative combine by order
  //h -> !0 h-> !1 h-> a
  def negativeSummarization(): Path = {
    //combine negative and positive cases in destination to negativeOR node

    var iindex = 0
    var ncells = Seq[Cell]()
    while (iindex < cells.length - 1) {

      val crrCell = cells(iindex)
      var cont = 0
      var jindex = iindex + 1

      while (jindex < cells.length && crrCell.equalsBySourceValue(cells(jindex))) {
        jindex += 1
        cont += 1
      }

      if (cont > 0) {
        val newTargetNodexindex = RegexNodeIndex.combineOrNegate(cells.slice(iindex, jindex).map(_.target))
        ncells :+= Cell(crrCell.i, crrCell.j, crrCell.source, newTargetNodexindex)
      }
      else {
        ncells :+= crrCell
      }

      iindex = jindex
    }

    if (iindex < cells.length) ncells = ncells :+ cells(iindex)

    cells = ncells
    this
  }


  //combine a positive path with a negative path by combination
  def combineNegativePath(negative: Path): Path = {

    val minSize = math.min(negative.cells.length, cells.length)
    val newPath = Path()
    var i = 0
    var found = false

    while (i < minSize && !found) {

      val negCell = negative.cells(i).regexify()
      val crrCell = cells(i).copy().regexify()

      //create target negative combinations
      val nodes = crrCell.negativeCombinations(negCell)
        .map(cell => (cell, cell.negateTarget()))
        .filter(_._2).map {
        _._1.source
      }

      if (nodes.isEmpty) {
        newPath.addCell(crrCell, crrCell.cost)
      }
      else {
        val cells = Regexify.searchPositives(nodes)
        newPath.addCell(cells)
        found = true
      }

      i += 1

    }

    if (i < cells.length)
      newPath.addCell(cells.slice(i, cells.length))
        .setNegative(found)
    else
      newPath.setNegative(found)

  }


  def toOrRegex(): RegexParam = {
    toOrRegex(cells, new RegexParam())
  }


  private def toOrRegex(cells: Seq[Cell], newParam: RegexParam): RegexParam = {

    val crrCell = cells.head
    val nextCells = cells.tail
    val prevCell = newParam.prevCell

    //need order between appends for reconstruction
    if (crrCell.isEqualWithoutIndex()) {

      newParam.sourceRemove(crrCell.source)
      newParam.targetRemove(crrCell.target)

      if (newParam.newDirection("equal")) {
        newParam.summarizePass()
      }

      if (newParam.containsSource(crrCell.source) || newParam.containsTarget(crrCell.target)) {
        newParam.sourceTop(crrCell.source)
        newParam.targetTop(crrCell.target)
      }
      else {
        newParam.addRegex(crrCell)
      }

    }
    else if (prevCell == null) {

      newParam.sourceTop(crrCell.source)
      newParam.targetTop(crrCell.target)

    }
    else if (crrCell.isCross(prevCell)) {


      if (newParam.newDirection("cross")) newParam.summarizePass()

      newParam.regexTransfer(crrCell.target)
      newParam.regexTransfer(crrCell.source)

      newParam.sourceTop(crrCell.source)
      newParam.targetTop(crrCell.target)

    }
    else if (prevCell.directional(crrCell) && crrCell.isDown(prevCell)) {

      newParam.newDirection("down")
      newParam.summarize()

      newParam.regexTransfer(crrCell.target)
      newParam.sourceTop(crrCell.source)


    }
    else if (prevCell.directional(crrCell) && crrCell.isRight(prevCell)) {

      newParam.newDirection("right")
      newParam.summarize()
      newParam.regexTransfer(crrCell.source)
      newParam.targetTop(crrCell.target)

    }


    if (!nextCells.isEmpty) {
      toOrRegex(nextCells, newParam.setPrevCell(crrCell))
    }
    else {
      newParam.newDirection("finish")
      newParam.summarize()
      newParam
    }

  }

  private def createOrderedRegex(cells: Seq[Cell], newParam: RegexParam): RegexParam = {

    val crrCell = cells.head
    val nextCells = cells.tail
    val prevCell = newParam.prevCell
    val dchange =


      if (crrCell.isEqualWithoutIndex()) {
        newParam.addRegex(crrCell)
        newParam.sourceRemove(crrCell.source)
        newParam.targetRemove(crrCell.target)

      }
      else if (prevCell == null) {
        newParam.sourceTop(crrCell.source)
        newParam.targetTop(crrCell.target)
      }
      else if (prevCell.directional(crrCell) && crrCell.isDown()) {
        newParam.sourceTop(crrCell.source)
        //newParam.addRegex(crrCell.target)
        //newParam.regexRemove(crrCell.source)
      }
      else if (prevCell.directional(crrCell) && crrCell.isRight()) {

        newParam.targetTop(crrCell.target)
        //newParam.addRegex(crrCell.source)
        //newParam.regexRemove(crrCell.target)
      }
      else if (crrCell.isCross()) {
        //newParam.removeRegex(crrCell)
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

  override def hashCode(): Int = super.hashCode()

  override def equals(obj: Any): Boolean = {
    if (canEqual(obj)) {
      val ins = obj.asInstanceOf[Path]
      val size = ins.cells.length

      if (size == cells.length) {
        return cells.zip(ins.cells).forall { case (cell1, cell2) => {
          cell1.equals(cell2)
        }
        }
      }
      false
    }
    else {
      false;
    }
  }

  override def canEqual(that: Any): Boolean = {
    that.isInstanceOf[Path]
  }
}
