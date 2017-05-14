package net.totietje.evaluator

import scala.collection.mutable.ListBuffer

/** An `Evaluator` is a string parser and evaluator.
  *
  * This class is flexible, allowing a user to define their own syntax. An example use might be to parse maths
  * expressions, such as `(1 + 2) ^ 3`.
  *
  * @see [[net.totietje.complex.ComplexEvaluator ComplexEvaluator]]
  * @tparam R
  *           What the string should be evaluated to
  */
abstract class Evaluator[R] {
  /**
    * Converts an expression into an array of [[net.totietje.evaluator.Token Tokens]].
    *
    * Each token has an assigned meaning. See the [[net.totietje.evaluator.Token Token]] for more detail on
    * how to create a token instance with a certain meaning.
    * @param expression
    *                   The input string to parse
    * @return
    *         An array of tokens containing the information needed to evaluate it
    */
  protected def tokenize(expression: String): Array[Token[R]]
  
  /** Parses a string.
    *
    * First of all, this tokenizes the string using the abstract `tokenize` method, which must be overridden
    * by the user. This transforms the string into an array of [[net.totietje.evaluator.Token Tokens]] which represent
    * the string in terms of its parts.
    *
    * Then, this evaluates the provided array of tokens as the user defines, producing a result of type `R`. For more
    * information on how this happens, see [[net.totietje.evaluator.Token Token]].
    * @throws net.totietje.evaluator.EvaluationException
    *                                                    If there is a syntax error in the expression
    * @param expression
    *                   The input string to parse
    * @return
    *         The result of parsing the input
    */
  final def evaluate(expression: String): R = {
    evaluateTokens(tokenize(expression))
  }
  
  private def evaluateTokens(tokens: Array[Token[R]]): R = {
    evaluatePostfix(ShuntingYard.toPostfix(tokens))
  }
  
  private def evaluatePostfix(tokens: Array[Token.Postfix[R]]): R = {
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
        case func: Token.Function[R] => func(for (_ <- 0 until func.args) yield pop()) +=: stack
        case value : Token.Value[R] => value() +=: stack
      }
    }
    
    if (stack.length != 1) {
      throw EvaluationException()
    }
    
    stack.last
  }
}