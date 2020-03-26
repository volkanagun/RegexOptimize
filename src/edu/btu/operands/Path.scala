package edu.btu.operands

import scala.util.control.Breaks


case class Path(var cells: Seq[Cell] = Seq(), var cost: Double = 0d) extends Serializable {

  var negative = false
  var multiple = false

  def setMultiple(multiple : Boolean):this.type = {
    this.multiple = multiple
    this
  }

  def getMultiple():Boolean = {
    multiple
  }

  def setNegative(negative:Boolean):this.type ={
    this.negative = negative
    this
  }

  def getNegative():Boolean ={
    negative
  }

  def addCell(cell: Cell, blockCost: Double): this.type = {
    cells :+= cell
    cost += blockCost
    this
  }

  def addCell(addCells: Seq[Cell]): this.type = {
    cells ++= addCells
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


  def negativePath(negative: Path): Path = {

    val minSize = math.min(negative.cells.length, cells.length)
    val newPath = Path()
    var i = 0
    var found = false

    while (i < minSize) {

      val negCell = negative.cells(i)
      val crrCell = cells(i).copy()
      val newCells = crrCell.negativeCombinations(negCell)
      val optCell = newCells.find(cell => cell.negativeSelf())

      optCell match {
        case Some(cell) => {
          newPath.addCell(cell, cell.cost)
          found = true
        }
        case None => {
          newPath.addCell(crrCell, crrCell.cost)
        }
      }

      i += 1

    }

    if (i < cells.length)
      newPath.addCell(cells.slice(i, cells.length))
        .setNegative(found)
    else
      newPath.setNegative(found)

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


  private def toOrRegex(cells: Seq[Cell], newParam: RegexParam): RegexParam = {

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