package net.totietje.complex

import net.totietje.complex.ComplexFunctionToken._
import net.totietje.evaluator.{AbstractTokenizer, Token}
import net.totietje.evaluator.Token.{CloseParen, OpenParen}

private object ComplexTokenizer extends AbstractTokenizer[ComplexFunction] {
  override protected def parseAfterValueChar(char: Char): Option[Token[ComplexFunction]] = char match {
    case '+' => Some(Plus)
    case '-' => Some(Minus)
    case '*' => Some(Multiply)
    case '/' => Some(Divide)
    case '^' => Some(Power)
    case ')' => Some(CloseParen())
    case _ => None
  }
  
  override protected def parseOtherChar(op: Char): Option[Token[ComplexFunction]] = op match {
    case '+' => Some(UnaryPlus)
    case '-' => Some(UnaryMinus)
    case '(' => Some(OpenParen())
    case _ => None
  }
  
  override protected def parseWord(word: String): Token[ComplexFunction] = word.toLowerCase match {
    case "i" => Constant(Complex.I)
    case "pi" | "π" => Constant(Complex.Pi)
    case "tau" | "τ" => Constant(Complex.Tau)
    case "e" => Constant(Complex.E)
    case "im" => Im
    case "re" => Re
    case "arg" => Arg
    case "abs" => Abs
    case "conj" => Conj
    case "sqrt" | "√" => Sqrt
    case "log" | "ln" => Log
    case "sin" => Sin
    case "asin" => Asin
    case "cos" => Cos
    case "acos" => Acos
    case "tan" => Tan
    case "atan" => Atan
    case "sinh" => Sinh
    case "asinh" => Asinh
    case "cosh" => Cosh
    case "acosh" => Acosh
    case "tanh" => Tanh
    case "atanh" => Atanh
    case _ => try {
      Constant(word.toDouble)
    } catch {
      case _: NumberFormatException => Variable(word)
    }
  }
}