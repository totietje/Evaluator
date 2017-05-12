package net.totietje.evaluator

import net.totietje.evaluator.ComplexFunctionToken._

object ComplexEvaluator extends Evaluator[ComplexFunction] {
  override protected def tokenize(expression: String): Array[Token] = {
    tokenize(expression, Array(), unary = true)
  }
  
  private def tokenize(expression: String, out: Array[Token], unary: Boolean) : Array[Token] = {
    if (expression.isEmpty) return out
    
    val first = expression.head
    if (isNumeric(first)) {
      readNumber(expression, out)
    } else if (isSpecialChar(first)) {
      tokenize(expression.substring(1), out :+ readSpecialChar(first, unary), first != ')')
    } else if (first.isWhitespace) {
      tokenize(expression.substring(1), out, unary)
    } else {
      readWord(expression, out)
    }
  }
  
  private def isNumeric(char: Char) : Boolean = char.isDigit || char == '.'
  
  private val specialChars = "+-*/^()"
  private def isSpecialChar(char: Char) : Boolean = specialChars.contains(char)
  
  private def readNumber(expression: String, out: Array[Token], acc: String = ""): Array[Token] = {
    if (expression.isEmpty) return out :+ parseNumber(acc)
    val first = expression.head
    if (isNumeric(first)) {
      readNumber(expression.substring(1), out, acc + first)
    } else {
      tokenize(expression, out :+ parseNumber(acc), unary = false)
    }
  }
  
  private def parseNumber(str: String) : Constant = try {
    Constant(str.toDouble)
  } catch {
    case _ : NumberFormatException => throw EvaluationException(s"Invalid token '$str'")
  }
  
  private def readSpecialChar(char: Char, unary: Boolean) : Token = char match {
    case '+' => if (unary) UNARY_PLUS else PLUS
    case '-' => if (unary) UNARY_MINUS else MINUS
    case '*' => MULTIPLY
    case '/' => DIVIDE
    case '^' => POWER
    case '(' => Token.OPEN_PAREN
    case ')' => Token.CLOSE_PAREN
  }
  
  private def readWord(expression: String, out: Array[Token], acc: String = ""): Array[Token] = {
    if (expression.isEmpty) {
      return out :+ toToken(acc)
    }
    
    val first = expression.head
    if (isNumeric(first) || isSpecialChar(first) || first.isWhitespace) {
      val token = toToken(acc)
      tokenize(expression, out :+ token, unary = !token.isInstanceOf[Token.Value[_]])
    } else {
      readWord(expression.substring(1), out, acc + first)
    }
  }
  
  private def toToken(acc: String) : Token = acc match {
    case "i"      => Constant(Complex.I)
    case "pi"|"π" => Constant(Complex.Pi)
    case "tau"    => Constant(Complex.Tau)
    case "e"      => Constant(Complex.E)
    case "im"     => IM
    case "re"     => RE
    case "arg"    => ARG
    case "abs"    => ABS
    case "sqrt"   => SQRT
    case "log"    => LOG
    case "sin"    => SIN
    case "asin"   => ASIN
    case "cos"    => COS
    case "acos"   => ACOS
    case "tan"    => TAN
    case "atan"   => ATAN
    case "sinh"   => SINH
    case "asinh"  => ASINH
    case "cosh"   => COSH
    case "acosh"  => ACOSH
    case "tanh"   => TANH
    case "atanh"  => ATANH
    case _        => Variable(acc)
  }
}
