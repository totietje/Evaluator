package net.totietje.evaluator

abstract class AbstractEvaluator[R] extends Evaluator[R] {
  override final protected def tokenize(expression: String): Array[Token[R]] = {
    tokenize(expression, Array(), unary = true)
  }
  
  private def tokenize(expression: String, out: Array[Token[R]], unary: Boolean) : Array[Token[R]] = {
    if (expression.isEmpty) return out
    
    val first = expression.head
    
    if (first.isWhitespace) {
      tokenize(expression.substring(1), out, unary)
    } else if (isNumeric(first)) {
      readNumber(expression, out)
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
  
  private def readNumber(expression: String, out: Array[Token[R]], acc: String = ""): Array[Token[R]] = {
    if (expression.isEmpty) return out :+ parseNumber(acc)
    val first = expression.head
    if (isNumeric(first)) {
      readNumber(expression.substring(1), out, acc + first)
    } else {
      tokenize(expression, out :+ parseNumber(acc), unary = false)
    }
  }
  
  private def readWord(expression: String, out: Array[Token[R]], acc: String = ""): Array[Token[R]] = {
    if (expression.isEmpty) {
      return out :+ parseWord(acc)
    }
    
    val first = expression.head
    if (isNumeric(first) || parseSpecialChar(first).isDefined || parseUnaryOperator(first).isDefined || first.isWhitespace) {
      val token = parseWord(acc)
      tokenize(expression, out :+ token, unary = !token.isInstanceOf[Token.Value[R]])
    } else {
      readWord(expression.substring(1), out, acc + first)
    }
  }
  
  protected def isNumeric(char: Char): Boolean
  protected def parseNumber(str: String): Token.Value[R]
  protected def parseSpecialChar(char: Char): Option[(Token[R], Boolean)]
  protected def parseUnaryOperator(op: Char): Option[Token[R]]
  protected def parseWord(acc: String) : Token[R]
}
