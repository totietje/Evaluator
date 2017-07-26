package net.totietje.evaluator

/** A skeletal implementation of the [[net.totietje.evaluator.Evaluator Evaluator]] class to avoid duplication
  * of common logic.
  *
  * This class is applicable if, and only if, for each character it is unambiguous whether it is part of a
  * word or special character. That is, every character can be either a special character, or part of a word, but not
  * both. Furthermore, neither special characters not words can contain whitespace.
  *
  * A 'special character' here is a standalone character that has a special meaning. Special characters cannot be part
  * of values or words. For example, `+` would be a special character.
  *
  * A 'word' here is a meaningful string of characters that do not contain any whitespace or special characters.
  * For example, the constant `pi` is a word, as are functions such as `sin`. Words cannot contain special characters
  * or they could be mistaken for two words - the word `a+b` would be split into `a`, `+`, and `b`.
  * @tparam R
  *           The type that the string input should be evaluated to
  */
abstract class AbstractEvaluator[R] extends Evaluator[R] {
  protected def tokenizer: Tokenizer[R]
  
  /** Parses a string.
    *
    * First of all, this tokenizes the string using the abstract `tokenizer` method, which must be overridden
    * by the user. This returns a [[net.totietje.evaluator.Tokenizer Tokenizer]] which breaks the string down into a
    * Seq of [[net.totietje.evaluator.Token Tokens]], representing the principle parts of
    * the string.
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
  final override def evaluate(expression: String): R = {
    evaluateTokens(tokenizer.tokenize(expression))
  }
  
  private def evaluateTokens(tokens: Seq[Token[R]]): R = {
    evaluatePostfix(ShuntingYard.toPostfix(tokens))
  }
  
  private def evaluatePostfix(tokens: Seq[Token.Postfix[R]], stack: Seq[R] = Seq()): R = {
    def stackIndex(index: Int) : R = {
      if (stack.isEmpty) {
        throw EvaluationException()
      }
      stack(index)
    }
    
    if (tokens.isEmpty) {
      if (stack.length != 1) {
        throw EvaluationException()
      } else {
        stack.head
      }
    } else {
      tokens.head match {
        case op: Token.Operator[R] => evaluatePostfix(tokens.tail, op(stackIndex(0), stackIndex(1)) +: stack.drop(2))
        case function: Token.Function[R] =>
          val functionResult = function(for (i <- 0 until function.args) yield stackIndex(i))
          evaluatePostfix(tokens.tail, functionResult +: stack.drop(function.args))
        case value : Token.Value[R] => evaluatePostfix(tokens.tail, value() +: stack.drop(1))
      }
    }
  }
}