package edu.btu.operands

import scala.util.control.Breaks


case class RegexNodeIndex(var indice: Int, var regexOp: RegexOp, var elems: Seq[RegexNodeIndex] = Seq()) {
  var matchTxt = ""
  var matchValue = ""
  var matchGroup = ""
  var index = 0


  regexOp.setContainer(this)

  def canEqual(that: Any): Boolean = canEqualMain(that) && that.asInstanceOf[RegexNodeIndex].indice == indice

  def canMatch(that: Any): Boolean = canMatchMain(that) && that.asInstanceOf[RegexNodeIndex].indice == indice


  def toRegexNodeIndex(): RegexNodeIndex = {
    RegexNodeIndex(0, regexOp, elems.map(_.toRegexNodeIndex()))
      .setMatchValue(matchValue)
      .setMatchTxt(matchTxt)
      .setMatchGroup(matchGroup)
  }

  def toRegexNodeIndex(indice: Int): RegexNodeIndex = {
    RegexNodeIndex(0, regexOp, elems.map(_.toRegexNodeIndex()))
      .setMatchValue(matchValue)
      .setMatchTxt(matchTxt)
      .setMatchGroup(matchGroup)
  }

  def isType(typeValue: String): Boolean = {
    (typeValue.equals(regexOp.name))
  }


  //what is the minimum change with minimum cost that prevents this match
  def negative(negativeNode: RegexNodeIndex): Boolean = {
    val matches = matchesByRegex(negativeNode)

    if (matches && !elems.isEmpty) {
      //remove this or elements or specialize `or` elements
      for (i <- 0 until elems.length) {
        if (elems(i).negative(negativeNode)) {
          return true
        }
      }
      false;
    }
    else if(matches && !equalsByText(negativeNode)){
      //specialize
      matchGroup = matchValue
      true
    }
    else if(matches){
      matchValue = "(?!" + negativeNode.matchTxt + ")" + matchValue
      true
    }
    else false

  }

  def setMatchTxt(matchTxt: String): this.type = {
    this.matchTxt = matchTxt
    this
  }

  def setMatchValue(matchValue: String): this.type = {
    this.matchValue = matchValue
    this
  }

  def setMatchGroup(matchGroup: String): this.type = {
    this.matchGroup = matchGroup
    this
  }

  def opName(): String = {
    regexOp.name
  }

  def toRegex(): String = {
    val (matchValue, matchGroup, mathcTxt) = Regexify.toRegex(this)
    matchValue
  }


  def copy(): RegexNodeIndex = {
    RegexNodeIndex(indice, regexOp.copy(), elems.map(rnode => rnode.copy()))
      .setMatchGroup(matchGroup).setMatchTxt(matchTxt).setMatchValue(matchValue)
  }

  def add(regexNode: RegexNodeIndex): this.type = {
    elems :+= regexNode
    this
  }

  def regexify(): this.type = {

    val (v, g, t) = Regexify.toRegex(this)

    matchValue = v
    matchGroup = g
    matchTxt = t
    regexOp = RegexOp(Regexify.seq, -1)

    this
  }


  override def toString: String = {
    toString(0)
  }

  def equalsByRegex(rightNode: RegexNodeIndex): Boolean = {
    matchValue.equals(rightNode.matchValue)
  }

  def equalsByGroup(rightNode: RegexNodeIndex): Boolean = {
    matchGroup.equals(rightNode.matchGroup)
  }

  def equalsByValue(rightNode: RegexNodeIndex): Boolean = {
    matchValue.equals(rightNode.matchValue)
  }

  def equalsByText(rightNode: RegexNodeIndex): Boolean = {
    matchTxt.equals(rightNode.matchTxt)
  }

  def matchesByGroup(rightNode: RegexNodeIndex): Boolean = {
    matchTxt.matches(rightNode.matchGroup)
  }

  def matchesByRegex(rightNode: RegexNodeIndex): Boolean = {
    matchTxt.matches(rightNode.matchValue)
  }

  def matchesByText(rightNode: RegexNodeIndex): Boolean = {
    matchTxt.matches(rightNode.matchTxt)
  }

  def canMatch(): Boolean = {
    if (matchTxt.isEmpty || matchGroup.isEmpty || matchValue.isEmpty) false
    else true
  }


  def canMatchMain(rightNodeAny: Any): Boolean = {
    val rightNode = rightNodeAny.asInstanceOf[RegexNodeIndex]
    equalsByRegex(rightNode) || equalsByGroup(rightNode) || matchesByRegex(rightNode) || matchesByGroup(rightNode)
  }

  def canEqualMain(rightNodeAny: Any): Boolean = {
    val rightNode = rightNodeAny.asInstanceOf[RegexNodeIndex]
    equalsByRegex(rightNode) /* || equalsByGroup(rightNode)*/

  }


  override def equals(obj: Any): Boolean = {
    if (canEqual(obj)) {
      val castObj = obj.asInstanceOf[RegexNodeIndex]
      castObj.regexOp.equals(regexOp)
    }
    else false
  }


  def toString(spaceCount: Int): String = {
    if (elems.length > 0) {
      val preText = s"RegexNode[" + regexOp.name + ","
      val preTextSpace = Regexify.spaceText(spaceCount + preText.length)
      preText + "\n" + preTextSpace + elems.map(_.toString(0)).mkString(",") + "\n" + Regexify.spaceText(spaceCount + 9) + "]"
    }
    else {
      Regexify.spaceText(spaceCount) + s"${regexOp.name} : ${matchValue}"
    }
  }
}

object RegexNode {
  def or(a: RegexNodeIndex, b: RegexNodeIndex): RegexNodeIndex = {
    if (a.canMatch(b)) RegexNodeIndex(a.indice, RegexOp(Regexify.seq))
      .setMatchGroup(a.matchGroup)
      .setMatchTxt(a.matchTxt)
      .setMatchValue(a.matchValue)
    else RegexNodeIndex(a.indice, RegexOp(Regexify.or), Seq(a, b))
      .regexify()
  }
}



