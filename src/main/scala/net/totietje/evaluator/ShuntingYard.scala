package net.totietje.evaluator

import Token._

private object ShuntingYard {
  def toPostfix[R](tokens: Seq[Token[R]], opStack: Seq[Precedence[R]] = Seq()): Seq[Postfix[R]] = {
    if (tokens.isEmpty) {
      emptyOpStack(opStack)
    } else {
      val (newOpStack: Seq[Precedence[R]], output: Seq[Postfix[R]]) = tokens.head match {
        case op: Operator[R] => addOp(op, opStack)
        case function: Function[R] => (function +: opStack, Seq())
        case value: Value[R] => (opStack, Seq(value))
        case ArgSeparator() => findOpenBracket(opStack)
        case paren: OpenParen[R] => (paren +: opStack, Seq())
        case CloseParen() =>
          val (newOpStack, output) = findOpenBracket(opStack)
          val (newerOpStack, nextOutput) = popParenthesis(newOpStack)
          (newerOpStack, output ++ nextOutput)
      }
      output ++ toPostfix(tokens.tail, newOpStack)
    }
  }
  
  private def emptyOpStack[R](opStack: Seq[Precedence[R]]): Seq[Postfix[R]] = {
    if (opStack.isEmpty) {
      Seq()
    } else {
      val top = opStack.head
      top match {
        case t: Postfix[R] => t +: emptyOpStack(opStack.tail)
        case _: Parenthesis[R] => throw EvaluationException("Mismatched parentheses")
      }
    }
  }
  
  private def addOp[R](op: Operator[R], opStack: Seq[Precedence[R]]): (Seq[Precedence[R]], Seq[Postfix[R]]) = {
    if (opStack.isEmpty) {
      (op +: opStack, Seq())
    } else {
      val top = opStack.head
      if ((top.precedence > op.precedence) || ((top.precedence == op.precedence) && op.associativity == Associativity.Left)) {
        val (newOpStack, output) = addOp(op, opStack.tail)
        (newOpStack, top.asInstanceOf[Postfix[R]] +: output)
      } else {
        (op +: opStack, Seq())
      }
    }
  }
  
  private def findOpenBracket[R](opStack: Seq[Token.Precedence[R]]): (Seq[Precedence[R]], Seq[Postfix[R]]) = {
    if (opStack.isEmpty) {
      throw EvaluationException("Mismatched parentheses")
    }
    
    val top = opStack.head
    if (!top.isInstanceOf[OpenParen[R]]) {
      val (newOpStack, output) = findOpenBracket(opStack.tail)
      (newOpStack, top.asInstanceOf[Postfix[R]] +: output)
    } else {
      (opStack, Seq())
    }
  }
  
  private def popParenthesis[R](opStack: Seq[Token.Precedence[R]]): (Seq[Precedence[R]], Seq[Postfix[R]]) = {
    val newOpStack = opStack.tail
    if (newOpStack.nonEmpty) {
      val top = newOpStack.head
      if (top.isInstanceOf[Function[R]]) {
        (newOpStack.tail, Seq(top.asInstanceOf[Postfix[R]]))
      }
    }
    (newOpStack, Seq())
  }
}