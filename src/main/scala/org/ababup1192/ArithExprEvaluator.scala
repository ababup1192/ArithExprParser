package org.ababup1192

object ArithExprEvaluator {
  def eval(ast: AST): Any = ast match {
    case AddOp(left, right) =>
      (eval(left), eval(right)) match {
        case (lval: Int, rval: Int) => lval + rval
      }
    case SubOp(left, right) =>
      (eval(left), eval(right)) match {
        case (lval: Int, rval: Int) => lval - rval
      }
    case MulOp(left, right) =>
      (eval(left), eval(right)) match {
        case (lval: Int, rval: Int) => lval * rval
      }
    case IntVal(value) => value
  }

}
