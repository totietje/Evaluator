package net.totietje.evaluator

import Token._
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

private object ShuntingYard {
  def toPostfix[R](tokens: Array[Token[R]]): Array[Postfix[R]] = {
    val opStack = new ListBuffer[Precedence[R]]()
    val output = new ArrayBuffer[Postfix[R]]()
    
    for (token <- tokens) {
      token match {
        case op: Operator[R] => addOp(op, opStack, output)
        case function: Function[R] => opStack += function
        case value: Value[R] => output += value
        case ArgSeparator() => findOpenBracket(opStack, output)
        case paren@OpenParen() => opStack += paren
        case CloseParen() =>
          findOpenBracket(opStack, output)
          popParenthesis(opStack, output)
      }
    }
    
    while (opStack.nonEmpty) {
      val token = opStack.remove(opStack.length - 1)
      token match {
        case t: Postfix[R] => output += t
        case _: Parenthesis[R] => throw EvaluationException("Mismatched parentheses")
      }
    }
    
    output.toArray
  }
  
  private def addOp[R](op: Operator[R], opStack: ListBuffer[Precedence[R]], output: ArrayBuffer[Postfix[R]]) : Unit = {
    if (opStack.isEmpty) {
      opStack += op
    } else {
      val top = opStack.last
      if ((top.precedence > op.precedence) || ((top.precedence == op.precedence) && op.associativity == Associativity.Left)) {
        output += opStack.remove(opStack.length - 1).asInstanceOf[Postfix[R]]
        addOp(op, opStack, output)
      } else {
        opStack += op
      }
    }
  }
  
  private def findOpenBracket[R](opStack: ListBuffer[Precedence[R]], output: ArrayBuffer[Postfix[R]]) : Unit = {
    var top = opStack.last
    while (!top.isInstanceOf[OpenParen[R]]) {
      opStack.remove(opStack.length - 1)
      output += top.asInstanceOf[Postfix[R]]
      if (opStack.isEmpty) throw EvaluationException("Mismatched parentheses")
      top = opStack.last
    }
  }
  
  private def popParenthesis[R](opStack: ListBuffer[Precedence[R]], output: ArrayBuffer[Postfix[R]]) = {
    opStack.remove(opStack.length - 1)
    if (opStack.nonEmpty) {
      val top = opStack.last
      if (top.isInstanceOf[Function[R]]) {
        output += top.asInstanceOf[Postfix[R]]
        opStack.remove(opStack.length - 1)
      }
    }
  }
}