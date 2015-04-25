package org.ababup1192

import scala.util.parsing.combinator.RegexParsers

trait AST

case class AddOp(left: AST, right: AST) extends AST

case class SubOp(left: AST, right: AST) extends AST

case class MulOp(left: AST, right: AST) extends AST

case class IntVal(value: Int) extends AST


object ArithExprParser extends RegexParsers {

  def intLiteral: Parser[AST] = """[1-9][0-9]*|0""".r ^^ {
    case value => IntVal(value.toInt)
  }

  def expr: Parser[AST] = chainl1(term, calc("+") | calc("-"))

  def calc(operand: String) = operand ^^ { op => (left: AST, right: AST) =>
    op match {
      case "+" => AddOp(left, right)
      case "-" => SubOp(left, right)
    }
  }

  def term: Parser[AST] = chainl1(factor, "*" ^^ { case op => (l: AST, r: AST) => MulOp(l, r) })

  def factor: Parser[AST] = intLiteral | "(" ~> expr <~ ")"

  def parse(input: String): ParseResult[Any] = parseAll(expr, input)
}
