package net.totietje.evaluator

abstract class AbstractTokenizer[R] extends Tokenizer[R] {
  /** Transforms an expression into a list of tokens.
    * @throws net.totietje.evaluator.EvaluationException
    *                                                    If the input string contains an invalid token
    * @param expression
    *                   The input string to parse
    * @return
    *         An array of tokens containing the information needed to evaluate it
    */
  override final def tokenize(expression: String): Seq[Token[R]] = {
    tokenize(expression, Seq(), afterValue = false)
  }
  
  private def tokenize(expression: String, out: Seq[Token[R]], afterValue: Boolean) : Seq[Token[R]] = {
    if (expression.isEmpty) return out
    
    val first = expression.head
    
    if (first.isWhitespace) {
      tokenize(expression.substring(1), out, afterValue)
    } else {
      val a = parseAfterValueChar(first)
      val b = parseOtherChar(first)
      
      (if (afterValue) (a, b) else (b, a)) match {
        case (None, None) => readWord(expression, out)
        case (Some(token), _)  => tokenize(expression.substring(1), out :+ token, isValue(token))
        case _ => throw EvaluationException(s"Char '$first' unexpected")
      }
    }
  }
  
  private def isValue(token: Token[R]) = token.isInstanceOf[Token.Value[R]] || token.isInstanceOf[Token.CloseParen[R]]
  
  private def readWord(expression: String, out: Seq[Token[R]], acc: String = ""): Seq[Token[R]] = {
    if (expression.isEmpty) {
      return out :+ parseWord(acc)
    }
    
    val first = expression.head
    if (parseAfterValueChar(first).isDefined || parseOtherChar(first).isDefined || first.isWhitespace) {
      val token = parseWord(acc)
      tokenize(expression, out :+ token, isValue(token))
    } else {
      readWord(expression.substring(1), out, acc + first)
    }
  }
  
  /** Determines if the character represents a token that comes after a value or close parenthesis.
    *
    * Operators belong here, as they come after values (for example, in `2 + 3` the `+` comes after the `2`).
    * Close parentheses also belong here, as do argument separators.
    *
    * In order to determine if a special character belongs here, ask yourself 'Would it make sense after a constant?'.
    * If the answer is yes, it belongs here.
    * @param char
    *             A character in the expression being parsed
    * @return
    *         `None` if the input is not a special character, or the character does not belong after a value.
    *         `Some(Token)` otherwise, where the token represents the character's purpose.
    */
  protected def parseAfterValueChar(char: Char): Option[Token[R]]
  
  /** Determines if the character represents a token that 1) comes after an operator, 2) comes after a function,
    * 3) follows an open parenthesis, or 4) is at the start of an expression.
    * start of an expression.
    *
    * Unary operators must be defined here, as they come after operators. The unary operator token must be
    * a [[net.totietje.evaluator.Token.Function Function]] token. A character can be both a binary operator (one
    * that works on two operands) and a unary operator (one that works on one operand), however, they must have
    * distinct tokens.
    *
    * Open parentheses also belong here, as they come after operators (for example, `2 * (3 + 4)`) or functions
    * (`sin(2)`). However, close parentheses do not, instead, they belong in the `parseAfterValueChar` method.
    *
    * In order to determine if a special character belongs here, ask yourself 'Would it make sense at the start of
    * an expression?' If the answer is yes, it belongs here.
    * @param char
    *           A character in the expression being parsed
    * @return
    *         `None` if the input is not a special character, or does not belong in the specified positions.
    */
  protected def parseOtherChar(char: Char): Option[Token[R]]
  
  /** Parses a possible word.
    * @throws net.totietje.evaluator.EvaluationException
    *                                                    If the input string is not a valid word
    * @param word
    *             The word to parse
    * @return
    *         The token representation of the word
    */
  protected def parseWord(word: String) : Token[R]
}
