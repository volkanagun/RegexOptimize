package edu.btu.search

import edu.btu.operands.{Cell, RegexNode, RegexNodeIndex, RegexOp, Regexify}

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


