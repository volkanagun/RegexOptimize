package edu.btu.operands

import scala.util.Random
import scala.util.control.Breaks


case class RegexNodeIndex(var maxDex: Int, var regexOp: RegexOp, var elems: Seq[RegexNodeIndex] = Seq()) extends Serializable {

  var matchTxt = ""
  var matchValue = ""
  var matchGroup = ""
  var sindex = 0
  var minDex = 0
  var fixedHash = -1
  var notNode = false
  var isTarget = false


  regexOp.setContainer(this)

  def setNegate(doNegate: Boolean = false): this.type = {
    this.notNode = doNegate
    this
  }

  def doNegate(): this.type = {
    this.notNode = true
    this.regexOp = RegexOp(Regexify.negate)
    this
  }

  def doTarget(): this.type = {
    this.isTarget = true
    this
  }

  def resetHash(): this.type = {
    fixedHash = -1
    hashCode()
    this
  }

  override def hashCode(): Int = {
    if (fixedHash == -1) {

      var fixedHash = 7
      fixedHash = fixedHash + 3 * maxDex
      fixedHash = fixedHash + 3 * sindex
      fixedHash = fixedHash + 3 * minDex
      fixedHash = fixedHash + 3 * regexOp.name.hashCode
      fixedHash = fixedHash + 3 * matchValue.hashCode
      fixedHash = fixedHash + 3 * isTarget.hashCode
      elems.foreach(e => fixedHash = fixedHash + 7 * e.hashCode())
      this.fixedHash = fixedHash
      this.fixedHash

    }
    else {
      fixedHash
    }
  }

  //this is important
  //randomize by grouping and regroup for generalization
  //randomize multiple times so check if already randomized
  def randomize(count: Int = 1): Seq[RegexNodeIndex] = {
    if (equalValueGroup()) {
      Seq(this)
    }
    else if (isSeq() && !isEmpty()) {

      val newElemGroups = for (i <- 0 until count;
                               indice = Random.nextInt(elems.length);
                               randomNodes = elems(indice).randomize();
                               elemGroups = randomNodes.map(randomNode => {
                                 (elems.slice(0, indice) :+ randomNode) ++ elems.slice(indice + 1, elems.length)
                               })) yield elemGroups

      newElemGroups.flatMap(crrGroup => crrGroup.map(new RegexNodeIndex(maxDex, regexOp, _).updateRegex().resetHash()))
    }
    else if(isOr() && !isEmpty()){
      //continuous grouping
      Seq(Regexify.continousOrGrouping(this))
    }
    else {
      val newMatchValue = matchGroup
      val newMatchGroup = matchGroup
      val newTxt = matchTxt
      val newNode = RegexNodeIndex(maxDex, regexOp, Seq())
        .setNegate(this.notNode).setMatchGroup(newMatchGroup)
        .setMatchTxt(newTxt)
        .setMatchValue(newMatchValue)
        .resetHash()

      Seq(this, newNode)
    }
  }

  def continuous(): RegexNodeIndex = {
    if (isSeq() && !isEmpty()) {
      Regexify.continousGrouping(this)
    }
    else {
      this
    }
  }

  override def equals(obj: Any): Boolean = {
    if (obj != null && obj.isInstanceOf[RegexNodeIndex]) {
      val castObj = obj.asInstanceOf[RegexNodeIndex]
      castObj.hashCode() == hashCode()
    }
    else false
  }

  def cntzMatchGroup(): String = {
    if (elems.length > 1)
      Regexify.specialize(matchGroup) + s"{${elems.length}}"
    else
      Regexify.specialize(matchGroup)
  }

  def symMatchGroup(): String = {
    Regexify.specialize(matchGroup)
  }

  def symMatchValue(): String = {
    Regexify.specialize(matchValue)
  }

  def symMatchTxt(): String = {
    Regexify.specialize(matchTxt)
  }

  def isEmpty(): Boolean = elems.isEmpty

  def getRndValue(rnd: Random): String = {
    if (rnd.nextBoolean()) matchValue else matchGroup
  }

  def getMatchValue(): String = {
    "(" + matchValue + ")"
  }

  def getMaxIndice(): Int = {
    if (!elems.isEmpty) {
      elems.maxBy(_.updateMaxIndice().getMaxDex())
        .maxDex
    }
    else {
      maxDex
    }
  }

  def getMinIndice(): Int = {
    if (!elems.isEmpty) {
      elems.minBy(_.updateMinIndice().getMinDex())
        .minDex
    }
    else {
      minDex
    }
  }

  def getMinDex(): Int = {
    if (elems.isEmpty) maxDex
    else minDex
  }

  def getMaxDex(): Int = {
    maxDex
  }

  def updateMinMaxIndice(): this.type = {
    updateMaxIndice()
    updateMinIndice()
  }

  def updateMaxIndice(): this.type = {
    maxDex = getMaxIndice()
    this
  }

  def updateMinIndice(): this.type = {
    minDex = getMinIndice()
    this
  }

  def setRegexOp(regexOp: RegexOp): this.type = {
    this.regexOp = regexOp
    this
  }

  def setRegexOpIfNotDefined(regexOp: RegexOp): this.type = {
    if (!this.regexOp.isDefined()) this.regexOp = regexOp
    this
  }

  def simplify(): RegexNodeIndex = {
    //down to elements! reduce the height of the tree
    if (elems.isEmpty) {
      this
    }
    else if (elems.length == 1) {
      val newNode = elems.head.setRegexOpIfNotDefined(regexOp)
      newNode.simplify()
    }
    else if (isSeq() || isEmpty()) {
      elems = elems.flatMap(elem => if (elem.isEmpty()) Seq(elem) else Seq(elem.simplify()))
      this
    }
    else {
      elems = elems.map(_.simplify())
      this
    }
  }

  def toCell(): Cell = {
    if (elems.length == 1) {
      elems.head.toCell()
    }
    else if (isOr()) {
      val source = elems.head
      val target = if (elems.length > 2) {
        RegexNodeIndex(sindex, regexOp, elems.tail)
      }
      else {
        elems.last
      }
      Cell(source.sindex, target.sindex, source, target)
    }
    else {
      Cell(sindex, sindex, this, this)
    }
  }


  def canEqual(that: Any): Boolean = canEqualMain(that) && that.asInstanceOf[RegexNodeIndex].maxDex == maxDex

  def canMatch(that: Any): Boolean = canMatchMain(that) && that.asInstanceOf[RegexNodeIndex].maxDex == maxDex

  def isOr(): Boolean = {
    Regexify.or.equals(regexOp.name) || Regexify.orgroup.equals(regexOp.name) || Regexify.orBracket.equals(regexOp.name) || Regexify.bracketCount.equals(regexOp.name)
  }

  def isOrNegate(): Boolean = {
    Regexify.ornegate.equals(regexOp.name)
  }

  def orNegateElems(): Seq[RegexNodeIndex] = {
    elems.filter(_.isOrNegate())
  }

  def negateElems(): Seq[RegexNodeIndex] = {
    elems.filter(_.notNode)
  }

  def equalValueGroup(): Boolean = {
    matchGroup.equals(matchValue)
  }

  def isSeq(): Boolean = {
    Regexify.group.equals(regexOp.name) || Regexify.seq.equals(regexOp.name)
  }

  def isOptional(): Boolean = {
    Regexify.optional.equals(regexOp.name) || Regexify.orbracketOptional.equals(regexOp.name)
  }

  def readyNode(): RegexNodeIndex = {
    if (isSeq()) this
    else {
      Regexify.toSeqNode(maxDex).add(this)
    }
  }


  def combineNode(regexNodeIndex: RegexNodeIndex): RegexNodeIndex = {
    readyNode().addOrder(regexNodeIndex)
  }

  //buggy here
  //recursive combine

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
      val nelems = Seq(this, node)
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

  def combineOr(node: RegexNodeIndex): RegexNodeIndex = {
    if (node.isOr() && (isSeq() || isOptional())) {
      val newNode = Regexify.toOrNode(node.sindex)
      newNode.elems ++= node.elems :+ this
      newNode
    }
    else if ((node.isSeq() || node.isOptional()) && isOr()) {
      val newNode = Regexify.toOrNode(node.sindex)
      newNode.elems ++= elems :+ node
      newNode
    }
    else if ((node.isSeq() && isSeq()) || (node.isOptional() && isOptional())) {
      Regexify.toOrNode(0, Seq(this, node))
    }
    else if (node.isOr() && isOr()) {
      Regexify.toOrNode(0, node.elems ++ elems)
    }
    else if (node.isOrNegate() || isOrNegate()) {
      val ornegates1 = node.negateElems()
      val ornegates2 = negateElems()

      val restelems1 = node.elems.toSet -- ornegates1.toSet
      val restelems2 = elems.toSet -- ornegates2.toSet

      val newOrNode = Regexify.toOrNode(0, (restelems1 ++ restelems2).toSeq)
      val newNegateNode = Regexify.toOrNode(0, (ornegates1 ++ ornegates2)).setNegate(true)
      Regexify.toOrNegateNode(0, Seq(newOrNode, newNegateNode))
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
  //3 cases if match by txt, regex, or group
  def negative(negativeNode: RegexNodeIndex): Boolean = {


    val equalByT = equalsByText(negativeNode)
    val matchesByR = if (!equalByT) matchesByRegex(negativeNode) else false
    val matchesByG = if (!matchesByR) matchesByGroup(negativeNode) else false

    if (equalByT) {
      //unable to modify
      return false
    }
    else if (matchesByR && !isEmpty()) {
      //can modify sub-elements
      return elems.exists(_.negative(negativeNode))
    }
    else if (matchesByR) {
      //unable to modify
      return false
    }
    else if (matchesByG) {
      //adding a negate element can modify using negation of match value (creates too much complexity skip it)
      /*

      add(RegexNodeIndex(negativeNode.maxDex, RegexOp(Regexify.seq), Seq())
        .setNot(true).setMatchValue(negativeNode.matchValue).setMatchTxt(negativeNode.matchTxt).setMatchGroup(negativeNode.matchGroup))
      add(RegexNodeIndex(maxDex, regexOp, Seq()).setMatchGroup(matchGroup).setMatchTxt(matchTxt).setMatchValue(matchValue))

      this.regexOp = RegexOp(Regexify.ornegate)

      updateRegex()
      */

      return false
    }
    else {
      //no need to modify already a negation
      //insert it


      negativeNode.doNegate()
      return true
    }

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


  def updateRegex(): this.type = {

    val (newMatchValue, newMatchGroup, newMatchTxt) = Regexify.toRegex(this)
    matchValue = newMatchValue
    matchTxt = newMatchTxt
    matchGroup = newMatchGroup
    this
  }

  def toCountGroup(min:Int, max:Int):String={
    matchGroup + "{"+min+","+max+"}"
  }

  def toRandomRegex(seed: Int): String = {
    val rndBoolean = new Random(seed).nextBoolean()
    val (matchValue, matchGroup, mathcTxt) = Regexify.toRegex(this)
    if (rndBoolean) matchValue else matchGroup
  }


  def copy(): RegexNodeIndex = {
    RegexNodeIndex(maxDex, regexOp.copy(), elems.map(rnode => rnode.copy()))
      .setMatchGroup(matchGroup).setMatchTxt(matchTxt).setMatchValue(matchValue).setNegate(notNode)
  }

  def add(regexNode: RegexNodeIndex): this.type = {
    elems :+= regexNode
    this
  }

  //recursively
  def addRecursive(regexNodeIndex: RegexNodeIndex): RegexNodeIndex = {
    if (!elems.isEmpty && getMinDex() >= regexNodeIndex.getMaxDex()) {
      elems = regexNodeIndex +: elems
      this
    }
    else if (!elems.isEmpty && getMaxDex() < regexNodeIndex.getMinDex()) {
      elems = elems :+ regexNodeIndex
      this
    }
    else if (!elems.isEmpty && getMaxDex() > regexNodeIndex.getMaxDex() && isSeq()) {
      addOrder(regexNodeIndex)
    }
    else if (!elems.isEmpty && isOr()) {
      elems.foreach(elemNode => {
        elemNode.addRecursive(regexNodeIndex)
      })

      this
    }
    else this

  }

  def addOrder(regexNode: RegexNodeIndex): this.type = {
    val pres = elems.takeWhile(p => p.maxDex < regexNode.maxDex)
    lazy val ress = elems.takeWhile(p => p.maxDex > regexNode.maxDex)

    if (pres.isEmpty && !elems.contains(regexNode)) elems = regexNode +: elems
    else if (ress.isEmpty && !elems.contains(regexNode)) elems = elems :+ regexNode
    else if (!pres.contains(regexNode) && !ress.contains(regexNode)) elems = (pres :+ regexNode) ++ ress
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

  def contains(newNode: RegexNodeIndex): Boolean = {
    equalsByHash(newNode) || elems.exists(e => e.contains(newNode))
  }

  def equalsByHash(righNode: RegexNodeIndex): Boolean = {
    hashCode() == righNode.hashCode()
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

  def matchesByInfGroup(rightNode: RegexNodeIndex): Boolean = {
    matchTxt.matches(rightNode.matchGroup+"+")
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

object RegexNodeIndex {

  def combineOrNegate(sequence: Seq[RegexNodeIndex]): RegexNodeIndex = {
    if (sequence.length == 1) sequence.head
    else {
      val minimumMaxDex = sequence.sortBy(_.maxDex).head.maxDex
      val containsNegative = sequence.exists(_.notNode)
      if (containsNegative) {
        RegexNodeIndex(minimumMaxDex, RegexOp(Regexify.ornegate), sequence).doNegate()
      }
      else {
        RegexNodeIndex(minimumMaxDex, RegexOp(Regexify.or), sequence)
      }

    }
  }
}



