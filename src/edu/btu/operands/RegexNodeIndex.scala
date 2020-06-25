package edu.btu.operands

import scala.util.Random
import scala.util.control.Breaks


case class RegexNodeIndex(var maxDex: Int, var regexOp: RegexOp, var elems: Seq[RegexNodeIndex] = Seq()) extends Serializable {

  var matchTxt = ""
  var matchValue = ""
  var matchGroup = ""
  var sindex = 0
  var minDex = 0

  regexOp.setContainer(this)

  def contains(regexNodeIndex: RegexNodeIndex):Boolean={
    if(elems.isEmpty) false
    else elems.exists(p=> p.equalsByRegex(regexNodeIndex))
  }

  def symMatchGroup():String={
    Regexify.specialize(matchGroup)
  }
  def symMatchValue():String={
    Regexify.specialize(matchValue)
  }

  def symMatchTxt():String={
    Regexify.specialize(matchTxt)
  }

  def isEmpty():Boolean=elems.isEmpty

  def getRndValue(rnd:Random):String={
    if(rnd.nextBoolean()) matchValue else matchGroup
  }

  def getMatchValue():String={
    "("+matchValue+")"
  }

  def getMaxIndice():Int = {
    if(!elems.isEmpty) {
      elems.maxBy(_.updateMaxIndice().getMaxDex())
        .maxDex
    }
    else{
      maxDex
    }
  }
  def getMinIndice():Int = {
    if(!elems.isEmpty) {
      elems.minBy(_.updateMinIndice().getMinDex())
        .minDex
    }
    else{
      minDex
    }
  }

  def getMinDex(): Int ={
    if(elems.isEmpty) maxDex
    else minDex
  }

  def getMaxDex():Int={
    maxDex
  }

  def updateMinMaxIndice():this.type ={
    updateMaxIndice()
    updateMinIndice()
  }

  def updateMaxIndice():this.type ={
    maxDex = getMaxIndice()
    this
  }
  def updateMinIndice():this.type ={
    minDex = getMinIndice()
    this
  }

  def setRegexOp(regexOp: RegexOp):this.type ={
    this.regexOp = regexOp
    this
  }

  def setRegexOpIfNotDefined(regexOp: RegexOp):this.type ={
    if(!this.regexOp.isDefined()) this.regexOp = regexOp
    this
  }

  def simplify():RegexNodeIndex={
    //down to elements! reduce the height of the tree
    if(elems.isEmpty){
      this
    }
    else if(elems.length == 1){
      val newNode = elems.head.setRegexOpIfNotDefined(regexOp)
      newNode.simplify()
    }
    else if(isSeq() || isEmpty()){
      elems = elems.flatMap(elem => if(elem.isEmpty()) Seq(elem) else Seq(elem.simplify()))
      this
    }
    else {
      elems = elems.map(_.simplify())
      this
    }
  }

  def toCell():Cell={
    if(elems.length == 1){
      elems.head.toCell()
    }
    else if(isOr()){
      val source = elems.head
      val target = if(elems.length > 2) {
        RegexNodeIndex(sindex, regexOp, elems.tail)
      }
      else {
        elems.last
      }
      Cell(source.sindex, target.sindex, source, target)
    }
    else{
      Cell(sindex, sindex, this, this)
    }
  }


  def canEqual(that: Any): Boolean = canEqualMain(that) && that.asInstanceOf[RegexNodeIndex].maxDex == maxDex

  def canMatch(that: Any): Boolean = canMatchMain(that) && that.asInstanceOf[RegexNodeIndex].maxDex == maxDex

  def isOr(): Boolean = {
    Regexify.or.equals(regexOp.name) || Regexify.orgroup.equals(regexOp.name) || Regexify.orBracket.equals(regexOp.name) || Regexify.bracketCount.equals(regexOp.name)
  }

  def isSeq(): Boolean = {
    Regexify.group.equals(regexOp.name) || Regexify.seq.equals(regexOp.name)
  }

  def isOptional(): Boolean = {
    Regexify.optional.equals(regexOp.name) || Regexify.orbracketOptional.equals(regexOp.name)
  }

  def readyNode():RegexNodeIndex={
    if(isSeq()) this
    else {
      Regexify.toSeqNode(maxDex).add(this)
    }
  }


  def combineNode(regexNodeIndex: RegexNodeIndex):RegexNodeIndex={
    readyNode().addOrder(regexNodeIndex)
  }

  //buggy here
  def combineOrNode(node: RegexNodeIndex): RegexNodeIndex = {
    if (node.isOr() && (isSeq() || isOptional())) {
      node.elems :+= this
      node
    }
    else if ((node.isSeq() || node.isOptional()) && isOr()) {
      elems :+= node
      this
    }
    else if ((node.isSeq() && isSeq()) || (node.isOptional() && isOptional())) {
      val nelems = Seq(this,  node)
      RegexNodeIndex(0, RegexOp(Regexify.or), nelems)
    }
    else if (node.isOr() && isOr()) {
      node.elems ++= elems
      node
    }
    else {
      null
    }
  }

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
  //if no change is needed return true
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
    else if (matches && !equalsByText(negativeNode)) {
      //specialize
      matchGroup = getMatchValue()
      true
    }
    else if (matches) {
      matchValue = "(?!" + negativeNode.matchTxt + ")" + getMatchValue()
      true
    }
    else true

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
  def toRandomRegex(seed:Int): String = {
    val rndBoolean = new Random(seed).nextBoolean()
    val (matchValue, matchGroup, mathcTxt) = Regexify.toRegex(this)
    if(rndBoolean) matchValue else matchGroup
  }


  def copy(): RegexNodeIndex = {
    RegexNodeIndex(maxDex, regexOp.copy(), elems.map(rnode => rnode.copy()))
      .setMatchGroup(matchGroup).setMatchTxt(matchTxt).setMatchValue(matchValue)
  }

  def add(regexNode: RegexNodeIndex): this.type = {
    elems :+= regexNode
    this
  }
  //recursively
  def addRecursive(regexNodeIndex: RegexNodeIndex):RegexNodeIndex={
    if(!elems.isEmpty && getMinDex() >= regexNodeIndex.getMaxDex()) {
      elems = regexNodeIndex +: elems
      this
    }
    else if(!elems.isEmpty && getMaxDex() < regexNodeIndex.getMinDex()){
      elems = elems :+ regexNodeIndex
      this
    }
    else if(!elems.isEmpty && getMaxDex() > regexNodeIndex.getMaxDex() && isSeq()){
      addOrder(regexNodeIndex)
    }
    else if (!elems.isEmpty && isOr())   {
      elems.foreach(elemNode=> {
        elemNode.addRecursive(regexNodeIndex)
      })

      this
    }
    else this

  }
  def addOrder(regexNode: RegexNodeIndex): this.type = {
    val pres = elems.takeWhile(p => p.maxDex < regexNode.maxDex)
    lazy val ress = elems.takeWhile(p => p.maxDex > regexNode.maxDex)
    if(pres.isEmpty) elems = regexNode +:elems
    else if(ress.isEmpty) elems = elems :+ regexNode
    else  elems = (pres :+ regexNode) ++ ress
    this
  }

  def regexify(): this.type = {

    val (v, g, t) = Regexify.toRegex(this)

    matchValue = v
    matchGroup = g
    matchTxt = t
    regexOp = RegexOp(regexOp.name, -1)

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
    matchTxt.matches(rightNode.getMatchValue())
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
    if (a.canMatch(b)) RegexNodeIndex(a.maxDex, RegexOp(Regexify.seq))
      .setMatchGroup(a.matchGroup)
      .setMatchTxt(a.matchTxt)
      .setMatchValue(a.matchValue)
    else RegexNodeIndex(a.maxDex, RegexOp(Regexify.or), Seq(a, b))
      .regexify()
  }
}



