package edu.btu.search

import edu.btu.operands.{Cell, Path, RegexNode, RegexNodeIndex, Regexify}
import edu.btu.task.tagmatch.TimeBox

import scala.util.control.Breaks

class SinglePositiveExact() extends AbstractRegexSearch() {

  override def regexify(value: String): RegexNodeIndex = Regexify.direct(value)

  override def search(): Seq[Matrix] = {

    val positiveZip1 = positives.zipWithIndex
    val positiveZip2 = positives.zipWithIndex

    val sourceTargets = positiveZip1.flatMap(pos1 => positiveZip2.map(pos2 => (pos1, pos2)))
      .filter { case (source, target) => source._2 > target._2 }.map { case (source, target) => (source._1, target._1) }

    val matrices = sourceTargets.map { case (source, target) => {

      val (sizex, sizey) = (source.length, target.length)
      val matrix = initMatrix(source, target)

      for (i <- 0 until sizex) {

        var prevMatching = 0
        val breaking = Breaks
        breaking.breakable {

          for (j <- 0 until sizey) {

            val cellContent = matrix(i)(j)

            if (source(i).equalsByValue(target(j))) {
              val cell = Cell(i, j)
              cell.source = source(i)
              cell.target = target(j)
              cell.matching = 0
              cell.cost = 0
              cellContent.addCell(cell)
              prevMatching = 0
            }
            else if (j == (sizey - 1)) {
              //last column
              //use continuous optional for source
              val cellOptional = Cell(i, j)
              cellOptional.source = source(i)
              cellOptional.target = target(j)
              cellOptional.matching = 3
              cellOptional.cost = 2
              cellContent.addCell(cellOptional)
            }
            else if (i == (sizex - 1)) {
              //last row
              //use continuous optional for target
              val cellOptional = Cell(i, j)
              cellOptional.source = source(i)
              cellOptional.target = target(j)
              cellOptional.matching = 4
              cellOptional.cost = 2
              cellContent.addCell(cellOptional)
            }
            else {

              //use optional
              val cellOptional = Cell(i, j)
              cellOptional.source = source(i)
              cellOptional.target = target(j)
              cellOptional.matching = 2
              cellOptional.cost = 2
              cellContent.addCell(cellOptional)

              //use or
              val cellOr = Cell(i, j)
              cellOr.source = source(i)
              cellOr.target = target(j)
              cellOr.matching = 1
              cellOr.cost = 1
              cellContent.addCell(cellOr)
              prevMatching = 1

            }
          }
        }

      }

      (source, target, matrix)
    }

    }

    matrices.map({ case (source, target, content) => Matrix(source, target, content) })
  }

  override def searchDirectional(): Seq[Path] = {

    TimeBox.measureTime[Seq[Path]]("search-exact-single-positive", searchDirectionalRegular(positives))

  }

  override def searchNegative(): Seq[Path] = {

    TimeBox.measureTime[Seq[Path]]("search-exact-single-positive", searchDirectionalRegular(negatives))

  }

}
