package net.totietje.evaluator

import scala.collection.mutable.{ArrayBuffer, ListBuffer}

abstract class Evaluator[R] {
  protected def tokenize(expression: String): Array[Token]
  
  final def parse(expression: String): R = {
    parse(tokenize(expression))
  }
  
  private def parse(tokens: Array[Token]): R = {
    parsePostfix(ShuntingYard.toPostfix(tokens))
  }
  
  private def parsePostfix(tokens: Array[Token.Postfix]): R = {
    val stack = ListBuffer[R]()
    
    def pop() : R = {
      if (stack.isEmpty) throw EvaluationException("Token expected")
      stack.remove(0)
    }
    
    for (token <- tokens) {
      token match {
        case op: Token.Operator[R] =>
          val a = pop()
          val b = pop()
          op(b, a) +=: stack
        case func: Token.Function[R] =>
          var popped = ArrayBuffer[R]()
          for (_ <- 0 until func.args) {
            popped += pop()
          }
          func(popped:_*) +=: stack
        case value : Token.Value[R] => value() +=: stack
      }
    }
    
    if (stack.length != 1) {
      throw EvaluationException()
    }
    
    stack.last
  }
}