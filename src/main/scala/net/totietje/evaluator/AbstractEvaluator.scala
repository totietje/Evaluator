package net.totietje.evaluator

/** A skeletal implementation of the [[net.totietje.evaluator.Evaluator Evaluator]] class to avoid duplication
  * of common logic.
  *
  * This class is applicable if, and only if, for each character it is unambiguous whether it is part of a value,
  * word, or special character. That is, every character can appear no more than once as a value, word, or special
  * character.
  *
  * A 'value' here is a string of characters that represent a value token. For example, `1` is a value, as is `2.3`.
  * Constants are ''not'' values, instead, they are words.
  *
  * A 'special character' here is a standalone character that has a special meaning. Special characters cannot be part
  * of values or words. For example, `+` is a special character.
  *
  * A 'word' here is a meaningful string of characters that do not contain any whitespace, special characters, or value
  * characters. For example, the constant `pi` is a word, as are functions such as `sin`.
  * @tparam R
  *           What the string should be evaluated to
  */
abstract class AbstractEvaluator[R] extends Evaluator[R] {
  override final protected def tokenize(expression: String): Array[Token[R]] = {
    tokenize(expression, Array(), unary = true)
  }
  
  private def tokenize(expression: String, out: Array[Token[R]], unary: Boolean) : Array[Token[R]] = {
    if (expression.isEmpty) return out
    
    val first = expression.head
    
    if (first.isWhitespace) {
      tokenize(expression.substring(1), out, unary)
    } else if (isValueChar(first)) {
      readValue(expression, out)
    } else if (unary) {
      parseUnaryOperator(first) match {
        case Some(op) => tokenize(expression.substring(1), out :+ op, unary = true)
        case None => readWord(expression, out)
      }
    } else {
      parseSpecialChar(first) match {
        case Some((token, nextUnary)) => tokenize(expression.substring(1), out :+ token, nextUnary)
        case None => readWord(expression, out)
      }
    }
  }
  
  private def readValue(expression: String, out: Array[Token[R]], acc: String = ""): Array[Token[R]] = {
    if (expression.isEmpty) return out :+ parseValue(acc)
    val first = expression.head
    if (isValueChar(first)) {
      readValue(expression.substring(1), out, acc + first)
    } else {
      tokenize(expression, out :+ parseValue(acc), unary = false)
    }
  }
  
  private def readWord(expression: String, out: Array[Token[R]], acc: String = ""): Array[Token[R]] = {
    if (expression.isEmpty) {
      return out :+ parseWord(acc)
    }
    
    val first = expression.head
    if (isValueChar(first) || parseSpecialChar(first).isDefined || parseUnaryOperator(first).isDefined || first.isWhitespace) {
      val token = parseWord(acc)
      tokenize(expression, out :+ token, unary = !token.isInstanceOf[Token.Value[R]])
    } else {
      readWord(expression.substring(1), out, acc + first)
    }
  }
  
  /** Determines whether the input is part of a value.
    * @param char
    *             The input character
    * @return
    *         Whether the input is part of a value
    */
  protected def isValueChar(char: Char): Boolean
  
  /** Parses a string of value characters.
    * @throws net.totietje.evaluator.EvaluationException
    *                                                    If the input string is not a valid value
    * @param str
    *            A string containing only value characters
    * @return
    *         The value of the string
    */
  protected def parseValue(str: String): Token.Value[R]
  
  /** Determines what special character the input is, if any.
    * @param char
    *             A possible special character
    * @return
    *         `None` if the input is not a special character, `Some(Token, Boolean)` if it is. The token
    *         is be the token representation of the character, and the boolean is whether this character was an
    *         operator.
    */
  protected def parseSpecialChar(char: Char): Option[(Token[R], Boolean)]
  
  /** Determines what unary operator the input is, if any.
    *
    * A unary operator, for the purpose of this function, is any char which follows another operator, as opposed
    * to a value. Thus, `(` is a unary operator. A character can be both a unary operator and a special character,
    * however, a distinction must be drawn in terms of its token ouput.
    * @param op
    *           A possible unary operator
    * @return
    *         `None` if the input is not a unary operator, `Some(Token)` if it is. The token
    *         is be the token representation of the character.
    */
  protected def parseUnaryOperator(op: Char): Option[Token[R]]
  
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
