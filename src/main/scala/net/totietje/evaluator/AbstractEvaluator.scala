package net.totietje.evaluator

/** A skeletal implementation of the [[net.totietje.evaluator.Evaluator Evaluator]] class to avoid duplication
  * of common logic.
  *
  * This class is applicable if, and only if, for each character it is unambiguous whether it is part of a
  * word or special character. That is, every character can be either a special character, or part of a word, but not
  * both.
  *
  * A 'special character' here is a standalone character that has a special meaning. Special characters cannot be part
  * of values or words. For example, `+` would be a special character.
  *
  * A 'word' here is a meaningful string of characters that do not contain any whitespace or special characters.
  * For example, the constant `pi` is a word, as are functions such as `sin`. Words cannot contain special characters
  * or they could be mistaken for two words - the word `a+b` would be split into `a`, `+`, and `b`.
  * @tparam R
  *           What the string should be evaluated to
  */
abstract class AbstractEvaluator[R] extends Evaluator[R] {
  /** Transforms an expression into a list of tokens.
    * @param expression
    *                   The input string to parse
    * @return
    *         An array of tokens containing the information needed to evaluate it
    */
  override final protected def tokenize(expression: String): Array[Token[R]] = {
    tokenize(expression, Array(), expectingValue = true)
  }
  
  private def tokenize(expression: String, out: Array[Token[R]], expectingValue: Boolean) : Array[Token[R]] = {
    if (expression.isEmpty) return out
    
    val first = expression.head
    
    if (first.isWhitespace) {
      tokenize(expression.substring(1), out, expectingValue)
    } else if (expectingValue) {
      parseAfterOperatorChar(first) match {
        case Some(op) => tokenize(expression.substring(1), out :+ op, expectingValue = true)
        case None => parseSpecialChar(first) match {
          case Some(_) => throw EvaluationException(s"Char '$first' unexpected")
          case None => readWord(expression, out)
        }
      }
    } else {
      parseSpecialChar(first) match {
        case Some((token, nextUnary)) => tokenize(expression.substring(1), out :+ token, nextUnary)
        case None => parseAfterOperatorChar(first) match {
          case Some(_) => throw EvaluationException(s"Char '$first' unexpected")
          case None => readWord(expression, out)
        }
      }
    }
  }
  
  private def readWord(expression: String, out: Array[Token[R]], acc: String = ""): Array[Token[R]] = {
    if (expression.isEmpty) {
      return out :+ parseWord(acc)
    }
    
    val first = expression.head
    if (parseSpecialChar(first).isDefined || parseAfterOperatorChar(first).isDefined || first.isWhitespace) {
      val token = parseWord(acc)
      tokenize(expression, out :+ token, expectingValue = !token.isInstanceOf[Token.Value[R]])
    } else {
      readWord(expression.substring(1), out, acc + first)
    }
  }
  
  /** Determines what special character the input is, if any.
    * @param char
    *             A possible special character
    * @return
    *         `None` if the input is not a special character, `Some(Token, Boolean)` if it is. The token
    *         is be the token representation of the character, and the boolean is whether this character was an
    *         operator.
    */
  protected def parseSpecialChar(char: Char): Option[(Token[R], Boolean)]
  
  /** Determines what token, if any, the input is, given that it comes after an operator, or at the start of the
    * expression.
    *
    * Unary operators must be defined here, as they come after operators. The unary operator token must be
    * a [[net.totietje.evaluator.Token.Function Function]] token. A character can be both a binary operator (one
    * that works on two operands) and a unary operator (one that works on one operand), however, they must have
    * distinct tokens.
    *
    * Open parentheses also belong here, as they come after operators (for example, `2 * (3 + 4)`). However, close
    * parentheses do not, they belong in the `parseSpecialChar` method.
    * @param op
    *           A character that follows an operator
    * @return
    *         `None` if the input is not a special character that follows an operator, `Some(Token)` if it is. The
    *         token is the token representation of the character.
    */
  protected def parseAfterOperatorChar(op: Char): Option[Token[R]]
  
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
