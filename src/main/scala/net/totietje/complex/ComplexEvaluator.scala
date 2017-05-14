package net.totietje.complex

import ComplexFunctionToken._
import net.totietje.evaluator.Token._
import net.totietje.evaluator.{AbstractEvaluator, EvaluationException, Token}

object ComplexEvaluator extends AbstractEvaluator[ComplexFunction] {
  override protected def isNumeric(char: Char): Boolean = char.isDigit || char == '.'
  
  override protected def parseNumber(str: String): Value[ComplexFunction] = try {
    Constant(str.toDouble)
  } catch {
    case _ : NumberFormatException => throw EvaluationException(s"Invalid token '$str'")
  }
  
  override protected def parseSpecialChar(char: Char): Option[(Token, Boolean)] = char match {
    case '+' => Some(PLUS, true)
    case '-' => Some(MINUS, true)
    case '*' => Some(MULTIPLY, true)
    case '/' => Some(DIVIDE, true)
    case '^' => Some(POWER, true)
    case ')' => Some(CLOSE_PAREN, false)
    case _   => None
  }
  
  override protected def parseUnaryOperator(op: Char): Option[Token] = op match {
    case '+' => Some(UNARY_PLUS)
    case '-' => Some(UNARY_MINUS)
    case '(' => Some(OPEN_PAREN)
    case _   => None
  }
  
  override protected def parseWord(acc: String): Token = acc.toLowerCase match {
    case "i"        => Constant(Complex.I)
    case "pi"|"π"   => Constant(Complex.Pi)
    case "tau"|"τ"  => Constant(Complex.Tau)
    case "e"        => Constant(Complex.E)
    case "im"       => IM
    case "re"       => RE
    case "arg"      => ARG
    case "abs"      => ABS
    case "sqrt"|"√" => SQRT
    case "log"      => LOG
    case "sin"      => SIN
    case "asin"     => ASIN
    case "cos"      => COS
    case "acos"     => ACOS
    case "tan"      => TAN
    case "atan"     => ATAN
    case "sinh"     => SINH
    case "asinh"    => ASINH
    case "cosh"     => COSH
    case "acosh"    => ACOSH
    case "tanh"     => TANH
    case "atanh"    => ATANH
    case _          => Variable(acc)
  }
}