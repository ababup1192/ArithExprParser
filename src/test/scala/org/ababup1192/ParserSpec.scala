package org.ababup1192

import org.scalatest._

class ParserSpec extends FlatSpec with Matchers {
  "ArithExpr parser" should "return AST" in {
    val parseResult = ArithExprParser.parse("((4 + 2) * 3) - 6")
    parseResult.get should be(SubOp(MulOp(AddOp(IntVal(4), IntVal(2)), IntVal(3)), IntVal(6)))
  }

  "ArithExpr evaluator" should "return value" in {
    val parseResult = ArithExprParser.parse("((4 + 2) * 3) - 6")
    ArithExprEvaluator.eval(parseResult.get) should be(12)
  }


}

