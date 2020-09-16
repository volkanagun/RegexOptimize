package edu.btu.search

import java.nio.file.Paths

import edu.btu.operands.{Cell, Path, RegexNode, RegexNodeIndex, RegexOp, Regexify}
import edu.btu.task.evaluation.ExperimentParams

import scala.util.Random
import scala.util.control.Breaks


abstract class AbstractRegexSearch() extends Serializable {

  var positives = Seq[Seq[RegexNodeIndex]]()
  var negatives = Seq[Seq[RegexNodeIndex]]()
  var additionalCost = 10d
  var rnd: Random = null

  //def search(): Seq[Matrix]

  def searchDirectional(): Seq[Path]

  def searchNegative(): Seq[Path]

  def regexify(value: String): Seq[RegexNodeIndex]

  def randomize(value: Set[String]): Set[String]

  def sizeControl(): this.type = {
    if (positives.length == 1) positives = Seq(positives.head, positives.head)
    if (negatives.length == 1) negatives = Seq(negatives.head, negatives.head)
    this
  }

  def initMatrix(source: Seq[RegexNodeIndex], target: Seq[RegexNodeIndex]): Array[Array[CellContent]] = {

    val sizex = source.length
    val sizey = target.length

    var matrix = Array[Array[CellContent]]()

    for (i <- 0 until sizex) {

      var row = Array[CellContent]()

      for (j <- 0 until sizey) {
        row = row :+ CellContent(i, j)
      }

      matrix = matrix :+ row

    }

    matrix

  }

  def initMatrix(sizex: Int, sizey: Int): Array[Array[CellContent]] = {

    var matrix = Array[Array[CellContent]]()
    for (i <- 0 until sizex) {

      var row = Array[CellContent]()

      for (j <- 0 until sizey) {
        row = row :+ CellContent(i, j)
      }
      matrix = matrix :+ row
    }
    matrix

  }

  def setRandom(rnd: Random): this.type = {
    this.rnd = rnd;
    this
  }

  def addPositiveNodes(seq: Seq[RegexNodeIndex]): this.type = {
    seq.foreach(item => {
      positives = positives :+ Seq(item)
    })
    this
  }

  def addPositive(samples: Seq[String]): this.type = {
    val sources = samples.flatMap(regexify(_).map(_.elems))
    positives ++= sources
    this
  }

  def addPositive(samples: Set[String]): this.type = {
    val sources = samples.flatMap(regexify(_).map(_.elems))
    positives ++= sources
    this
  }

  def setPositive(samples: Set[String]): this.type = {
    val sources = samples.flatMap(regexify(_).map(_.elems)).toSeq
    positives = sources
    this
  }

  def addPositive(sample: String): this.type = {
    val source = regexify(sample)
    positives ++= source.map(_.elems)
    this
  }

  def addNegative(samples: Seq[String]): this.type = {
    val sources = samples.flatMap(regexify(_).map(_.elems))
    negatives ++= sources
    this
  }


  def addNegative(samples: Set[String]): this.type = {
    val sources = samples.flatMap(regexify(_).map(_.elems))
    negatives ++= sources
    this
  }

  def setNegative(samples: Set[String]): this.type = {
    negatives = Seq[Seq[RegexNodeIndex]]()
    addNegative(samples)
    this
  }

  def addNegative(sample: String): this.type = {
    val source = regexify(sample).map(_.elems)
    negatives ++= source
    this
  }


  def search(matrices: Seq[Matrix]): Seq[Seq[Path]] = {
    matrices.map(matrix => {
      val cells = matrix.getCells(0, 0)
      val paths = cells.flatMap(search(_, Seq(), matrix))
      paths
    })
  }

  def search(matrices: Seq[Matrix], top: Int): Seq[Seq[Path]] = {
    matrices.map(matrix => {
      val cells = matrix.getCells(0, 0)
      val paths = cells.flatMap(search(_, Seq(), matrix, top))
      paths
    })
  }

  def searchLoop(matrices: Seq[Matrix], top: Int): Seq[Seq[Path]] = {
    matrices.map(matrix => {
      val cells = matrix.getCells(0, 0)
      val paths = cells.flatMap(searchLoop(_, matrix, top))
      paths
    })
  }

  def searchZigZagLoop(matrices: Seq[Matrix], top: Int): Seq[Seq[Path]] = {
    val zigzagCost: Double = 10.0
    matrices.map(matrix => {
      val cells = matrix.getCells(0, 0)
      val paths = cells.flatMap(searchZigZagLoop(_, matrix, top, zigzagCost))
      paths
    })
  }

  protected def search(newCell: Cell, paths: Seq[Path], matrix: Matrix): Seq[Path] = {

    val updatePaths = if (paths.isEmpty) Seq(Path(Seq(newCell), newCell.cost))
    else paths.map(currentPath => currentPath.copy().addCell(newCell, newCell.cost))

    //while(i, j) until block is distrupted...
    //when disrupted use recursion to create new block on the last path
    updatePaths.flatMap(currentPath => {
      val currentCell = currentPath.getLastCell()
      val nextCells = currentCell.nextCells(matrix)
      if (nextCells.isEmpty) updatePaths
      else {
        nextCells.flatMap(nextCell => search(nextCell, Seq(currentPath), matrix))
      }
    }).sortBy(_.cost)
  }

  protected def search(newCell: Cell, paths: Seq[Path], matrix: Matrix, top: Int): Seq[Path] = {

    //updating current paths by adding or modifying the last cell
    val updatePaths = if (paths.isEmpty) Seq(Path(Seq(newCell), newCell.cost))
    else paths.map(currentPath => currentPath.copy().addCell(newCell, newCell.cost))

    //last blocks of the updatepaths
    val updateCells = newCell.nextCells(matrix)

    val returnPaths = (if (updateCells.isEmpty) updatePaths
    else updateCells.flatMap(updateCell => search(updateCell, updatePaths, matrix, top)))

    returnPaths.sortBy(_.cost).take(top)
  }

  protected def searchLoop(newCell: Cell, matrix: Matrix, top: Int): Seq[Path] = {

    //updating current paths by adding or modifying the last cell

    var updatePaths = Seq(Path(Seq(newCell), newCell.cost))
    var updateCelling = newCell.nextCells(matrix).map(cell => (cell, Path(Seq(newCell), newCell.cost)))


    while (!updateCelling.isEmpty) {

      val newCelling = updateCelling.map { case (cell, currentPath) => {
        val newPath = currentPath.copy().addCell(cell, cell.cost)
        (cell, newPath)
        //for this cell and path
      }
      }

      updatePaths = newCelling.map(_._2).sortBy(_.cost)
      updateCelling = newCelling
        .flatMap { case (cell, path) => {
          cell.nextCells(matrix).map(newCell => (newCell, path))
        }
        }
        .sortBy(_._2.cost).take(top)
    }

    updatePaths

  }

  protected def searchZigZagLoop(newCell: Cell, matrix: Matrix, top: Int, cost: Double): Seq[Path] = {

    //updating current paths by adding or modifying the last cell

    var updatePaths = Seq(Path(Seq(newCell), newCell.cost))
    var updateCelling = newCell.nextCells(matrix).map(cell => (cell, Path(Seq(newCell), newCell.cost)))


    while (!updateCelling.isEmpty) {

      val newCelling = updateCelling.map { case (cell, currentPath) => {
        val newPath = currentPath.copy().addCell(cell, cell.cost, cost)
        (cell, newPath)
        //for this cell and path
      }
      }

      updatePaths = newCelling.map(_._2)
      updateCelling = newCelling
        .flatMap { case (cell, path) => {
          cell.nextCells(matrix).map(newCell => (newCell, path))
        }
        }
        .sortBy(_._2.cost).take(top)

    }

    updatePaths

  }

  def goDown(crrCell: Cell, rowLength: Int): (Boolean, Int, Int) = {
    if (crrCell.i < (rowLength - 1)) (true, crrCell.i + 1, crrCell.j)
    else (false, 0, 0)
  }

  def goRight(crrCell: Cell, columnLength: Int): (Boolean, Int, Int) = {
    if (crrCell.j < (columnLength - 1)) (true, crrCell.i, crrCell.j + 1)
    else (false, 0, 0)
  }

  def goCross(crrCell: Cell, rowLength: Int, columnLength: Int): (Boolean, Int, Int) = {
    if (crrCell.i < (rowLength - 1) && crrCell.j < (columnLength - 1)) (true, crrCell.i + 1, crrCell.j + 1)
    else (false, 0, 0)
  }

  protected def searchDirectional(paths: Seq[Path], source: Seq[RegexNodeIndex], target: Seq[RegexNodeIndex], i: Int, j: Int): Seq[Path] = {
    paths.flatMap(searchDirectional(_, source, target, i, j))
  }

  def searchCost(path: Path, cell: Cell): Double = {
    val sourceNode = cell.source
    val targetNode = cell.target

    if (path.hasLastCell()) {
      val lastCell = path.getLastCell()

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

  //looks buggy looks fixed
  protected def searchDirectional(path: Path, source: Seq[RegexNodeIndex], target: Seq[RegexNodeIndex], i: Int, j: Int): Seq[Path] = {

    val sourceLength = source.length
    val targetLength = target.length
    val sourceNode = source(i)
    val targetNode = target(j)
    val cell = Cell(i, j, sourceNode, targetNode)

    var paths = Set[Path]()
    val blockCost = searchCost(path, cell)

    val nextPath = path.addCell(cell, blockCost, additionalCost)

    if (cell.isLast(sourceLength, targetLength)) {
      paths = paths + nextPath
    }

    //multi
    if (path.hasLastCell()) {

      val lastCell = path.getLastCell()

      if (lastCell.isDown()) {

        val (canDown, di, dj) = goDown(cell, sourceLength)
        val (canCross, ci, cj) = goCross(cell, sourceLength, targetLength)
        if (canDown) paths ++= searchDirectional(path.copy(), source, target, di, dj)
        if (canCross) paths ++= searchDirectional(path.copy(), source, target, ci, cj)

      }

      if (lastCell.isRight()) {

        val (canRight, di, dj) = goRight(cell, targetLength)
        val (canCross, ci, cj) = goCross(cell, sourceLength, targetLength)
        if (canRight) paths ++= searchDirectional(path.copy(), source, target, di, dj)
        if (canCross) paths ++= searchDirectional(path.copy(), source, target, ci, cj)

      }

      if (lastCell.isCross()) {

        val (canRight, ri, rj) = goRight(cell, target.length)
        val (canDown, di, dj) = goDown(cell, source.length)
        val (canCross, ci, cj) = goCross(cell, source.length, target.length)

        if (canRight) paths ++= searchDirectional(path.copy(), source, target, ri, rj)
        if (canDown) paths ++= searchDirectional(path.copy(), source, target, di, dj)
        if (canCross) paths ++= searchDirectional(path.copy(), source, target, ci, cj)

      }

    }
    else {

      val (canRight, ri, rj) = goRight(cell, target.length)
      val (canDown, di, dj) = goDown(cell, source.length)
      val (canCross, ci, cj) = goCross(cell, source.length, target.length)

      if (canRight) paths ++= searchDirectional(path.copy(), source, target, ri, rj)
      if (canDown) paths ++= searchDirectional(path.copy(), source, target, di, dj)
      if (canCross) paths ++= searchDirectional(path.copy(), source, target, ci, cj)

    }

    //take the top 3 least costs
    //for efficiency
    paths.toSeq

  }



  protected def pathBinning1(paths: Seq[Path]):Seq[Path] = {

    val headMap = paths.sortBy(_.cost)
      .groupBy(path => path.cost)

    var seq = Seq[Path]()
    var index = 0;
    var count = 0;

    while(count < 1) {

      if(headMap.contains(index)){

        val hh = headMap(index).head
        val ll = headMap(index).last

        seq = seq :+ hh
        seq = seq :+ ll
        count+=1

      }

      index += 1
    }

    seq
  }



  protected def pathBinning(paths: Seq[Path], size:Int = 1):Seq[Path] = {
    paths.sortBy(_.cost).take(1)
  }

  protected def searchDirectionalRegular(regexNodes: Seq[Seq[RegexNodeIndex]], maxSinglePath:Int): Seq[Path] = {

    val nodeZip1 = regexNodes.zipWithIndex
    val nodeZip2 = regexNodes.zipWithIndex

    val sourceTargets = nodeZip1.par.flatMap(pos1 => nodeZip2.map(pos2 => (pos1, pos2)))
      .filter { case (source, target) => source._2 > target._2 }
      .map { case (source, target) => (source._1, target._1) }

    val paths = sourceTargets.flatMap { case (source, target) => {
      pathBinning(searchDirectional(Path(), source, target, 0, 0))
    }}

    paths.toArray.sortBy(_.cost)
      .take(maxSinglePath)

  }

  //find the minimum change, and apply it for rejecting the negative
  /**
   * Summarize positive and negative and combine them into multiple paths with minimum change
   *
   * @return
   */
  def searchDirectionalNegativeOriginal(): Seq[Path] = {
    val pathPositives = searchDirectional()
    val pathNegatives = searchNegative()

    val pathCombined = pathPositives.flatMap(positive => {
      var paths = Seq(positive, positive.swap())
      var newSet = Set[Path]()

      for (k <- 0 until paths.size) {
        for (i <- 0 until pathNegatives.size) {
          newSet = newSet ++ paths(k).negativePath(pathNegatives(i))
          newSet = newSet ++ paths(k).negativePath(pathNegatives(i).swap())
        }
      }

      newSet
    }).filter(_.negative)
      .distinct
      .toArray

    pathCombined
  }

  def searchNegativeRegex(): Set[String] = {

    val pathPositives = searchDirectional().sortBy(_.cost)
    val pathNegatives = searchNegative()

    val negativeRegexes = pathNegatives.map(path=> path.toOrRegex().createRegexNode())
      .map(_.toRegex()).toSet.mkString("(?!","|",")")

    val positiveRegexes = pathPositives.map(path=> path.toOrRegex().createRegexNode())
      .map(_.toRegex()).toSet

   positiveRegexes.map(posReg=> negativeRegexes + posReg)

  }


  def createNode(leftPath: Path, rightPath: Path): RegexNodeIndex = {
    val leftNode = leftPath.toOrRegex().constructRegexNode()
    val rightNode = rightPath.toOrRegex().constructRegexNode()
    leftNode.combineOrNode(rightNode)
  }


  //randomize and combine path pairs
  protected def searchMultiDirectional(regexNodes: Seq[Seq[RegexNodeIndex]], maxSinglePath:Int,  crrDepth: Int = 1) : Seq[Path] = {

    val random = new Random(19)
    val minMax = math.max(regexNodes.length, 1)

    val sourceIndices = Range(0, minMax)
      .map { index => random.nextInt(regexNodes.length) }

    val targetIndices = Range(0, regexNodes.length)
      .filter(!sourceIndices.contains(_))

    val sourceSeq = sourceIndices.map(i => regexNodes(i))
    val targetSeq = targetIndices.map(i => regexNodes(i))

    val sourceTargets = sourceSeq.flatMap(pos1 => targetSeq.map(pos2 => (pos1, pos2)))

    val allpaths = sourceTargets.flatMap { case (src, trt) => {
      pathBinning(searchDirectional(Path(), src, trt, 0, 0))
    }}

    if (allpaths.length <= 2 || crrDepth == ExperimentParams.maxMultiDepth || allpaths.length > regexNodes.length) {
      allpaths.sortBy(_.cost).take(maxSinglePath)
    }
    else {
      val nodes = sortTakeConstruct(allpaths)
      searchMultiDirectional(nodes, maxSinglePath, crrDepth + 1)
    }

  }

  def sortTake(paths: Seq[Path]): Seq[Path] = {
    paths.sortBy(_.cost)
      .take(2*ExperimentParams.maxPaths)
  }

  def sortTakeConstruct(paths: Seq[Path]): Seq[Seq[RegexNodeIndex]] = {
    paths.sortBy(_.cost)
      .take(2 * ExperimentParams.maxPaths)
      .map(path => Seq(path.toOrRegex().constructRegexNode()))
  }

  def searchRegex(paths: Seq[Path]): Seq[String] = {

    val regexNodes = paths.map(crrPath => {
      crrPath.toOrRegex().constructRegexNode()
    }).distinct

    val indices = for (x <- 0 until regexNodes.length; y <- 0 until regexNodes.length) yield (x, y)
    val elems = indices.filter { case (x, y) => x != y }
      .map { case (x, y) => (regexNodes(x).combineOrNode(regexNodes(y))) }

    val regexes = elems.map(nodeIndex => nodeIndex.toRegex())
    regexes

  }

  def searchPositiveRegex(): Seq[String] = {
    val paths = searchDirectional()
    searchRegex(paths)
  }

  /*def searchNegativeRegex(): Seq[String] = {
    val paths = searchDirectionalNegative()
    searchRegex(paths)
  }*/

}
