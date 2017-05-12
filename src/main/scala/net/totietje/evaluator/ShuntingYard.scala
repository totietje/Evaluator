package net.totietje.evaluator

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

private object ShuntingYard {
  def toPostfix[R](tokens: Array[Token]): Array[Token.Postfix] = {
    val opStack = new ListBuffer[Token.Precedence]()
    val output = new ArrayBuffer[Token.Postfix]()
    
    for (token <- tokens) {
      token match {
        case op: Token.Operator[_] => addOp(op, opStack, output)
        case function: Token.Function[_] => opStack += function
        case value: Token.Value[_] => output += value
        case Token.ARG_SEPARATOR => findOpenBracket(opStack, output)
        case Token.OPEN_PAREN => opStack += Token.OPEN_PAREN
        case Token.CLOSE_PAREN =>
          findOpenBracket(opStack, output)
          popParenthesis(opStack, output)
      }
    }
    
    while (opStack.nonEmpty) {
      val token = opStack.remove(opStack.length - 1)
      token match {
        case _: Token.Parenthesis => throw EvaluationException("Mismatched parentheses")
        case t: Token.Postfix => output += t
      }
    }
    
    output.toArray
  }
  
  private def addOp[R](op: Token.Operator[R], opStack: ListBuffer[Token.Precedence], output: ArrayBuffer[Token.Postfix]) : Unit = {
    if (opStack.isEmpty) {
      opStack += op
    } else {
      val top = opStack.last
      if ((top.precedence > op.precedence) || ((top.precedence == op.precedence) && op.associativity == Associativity.LEFT)) {
        output += opStack.remove(opStack.length - 1).asInstanceOf[Token.Postfix]
        addOp(op, opStack, output)
      } else {
        opStack += op
      }
    }
  }
  
  private def findOpenBracket(opStack: ListBuffer[Token.Precedence], output: ArrayBuffer[Token.Postfix]) : Unit = {
    var top = opStack.last
    while (top != Token.OPEN_PAREN) {
      opStack.remove(opStack.length - 1)
      output += top.asInstanceOf[Token.Postfix]
      if (opStack.isEmpty) throw EvaluationException("Mismatched parentheses")
      top = opStack.last
    }
  }
  
  private def popParenthesis(opStack: ListBuffer[Token.Precedence], output: ArrayBuffer[Token.Postfix]) = {
    opStack.remove(opStack.length - 1)
    if (opStack.nonEmpty) {
      val top = opStack.last
      if (top.isInstanceOf[Token.Function[_]]) {
        output += top.asInstanceOf[Token.Postfix]
        opStack.remove(opStack.length - 1)
      }
    }
  }
}