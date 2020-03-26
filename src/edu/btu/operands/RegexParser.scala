package edu.btu.operands

import com.sun.org.apache.xpath.internal.operations.Or

import scala.util.parsing.combinator.JavaTokenParsers


trait ArithAST {
  sealed abstract class Expr
  case class Add(e1: Expr, e2: Expr) extends Expr
  case class Sub(e1: Expr, e2: Expr) extends Expr
  case class Mul(e1: Expr, e2: Expr) extends Expr
  case class Div(e1: Expr, e2: Expr) extends Expr
  case class Number(e: String) extends Expr
}

trait GroupAST {

  sealed abstract class Expr
  case class Or(e1: Expr, e2: Expr) extends Expr
  case class Seq(e1: Expr, e2:Expr) extends Expr
  case class SeqOpt(e1: Expr, e2:Expr) extends Expr
  case class Bracket(e1: Expr) extends Expr
  case class BracketCount(e1: Expr) extends Expr
  case class Elem(e:Char) extends Expr
  case class Slash() extends Expr

}

trait RegexParser extends JavaTokenParsers with GroupAST {


  def elem:Parser[Expr] = Elem | "\\" ~ Elem
  def expr: Parser[Expr] = chainl1(elem, "|" ^^^ Or)
  def group:Parser[Expr] = "(" ~> expr <~ ")"
  def bracket = Elem | "(" ~> expr <~ ")"

  /*def factor = floatingPointNumber ^^ Number | "(" ~> expr <~ ")"*/
}

trait ArithParser extends JavaTokenParsers with ArithAST {
  def expr: Parser[Expr] = chainl1(term, "+" ^^^ Add | "-" ^^^ Sub)
  def term = chainl1(factor, "*" ^^^ Mul | "/" ^^^ Div)
  def factor = floatingPointNumber ^^ Number | "(" ~> expr <~ ")"
}

object ArithParserCLI extends ArithParser {

  def main(args: Array[String]) {
    for (arg <- args) {
      println("input: " + arg)
      println("output: " + parseAll(expr, arg))
    }
  }
}