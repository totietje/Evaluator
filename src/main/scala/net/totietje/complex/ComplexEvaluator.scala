package net.totietje.complex

import ComplexFunctionToken._
import net.totietje.evaluator.Token._
import net.totietje.evaluator.{AbstractEvaluator, Token}

/** An [[net.totietje.evaluator.Evaluator Evaluator]] which parses an expression as a
  * [[net.totietje.complex.ComplexFunction ComplexFunction]].
  *
  * This allows the basic operators `+`, `-`, `*`, `/` and `^` and the use of brackets.
  *
  * The following strings are treated as constants:
  *   - i
  *   - pi or π
  *   - tau or τ
  *   - e
  *
  * Furthermore, the following functions are recognised:
  *   - im
  *   - re
  *   - arg
  *   - abs
  *   - sqrt or √
  *   - log or ln
  *   - sin
  *   - asin
  *   - cos
  *   - acos
  *   - tan
  *   - atan
  *   - sinh
  *   - asinh
  *   - cosh
  *   - acosh
  *   - tanh
  *   - atanh
  *
  * There is currently no support for implicit multiplication without the `*` symbol, such as `2i`.
  *
  * A string can be parsed like this:
  * {{{
  *   val complexFunction = ComplexEvaluator.evaluate("2 + i * sin(x)")
  * }}}
  *
  * That returned `ComplexFunction` can then be evaluated for a given input for `x` with:
  * {{{
  *   val complexNumber = complexFunction(Map("x" -> xValue))
  * }}}
  *
  * For example, replacing `xValue` with `Complex.Pi` would produce a complex number roughly equal to 2.
  * It is recommended to then round the result by calling `.round` to round the number to an appropriate degree
  * of accuracy.
  */
object ComplexEvaluator extends AbstractEvaluator[ComplexFunction] {
  
  override protected def parseSpecialChar(char: Char): Option[(Token[ComplexFunction], Boolean)] = char match {
    case '+' => Some(PLUS, true)
    case '-' => Some(MINUS, true)
    case '*' => Some(MULTIPLY, true)
    case '/' => Some(DIVIDE, true)
    case '^' => Some(POWER, true)
    case ')' => Some(CloseParen(), false)
    case _   => None
  }
  
  override protected def parseAfterOperatorChar(op: Char): Option[Token[ComplexFunction]] = op match {
    case '+' => Some(UNARY_PLUS)
    case '-' => Some(UNARY_MINUS)
    case '(' => Some(OpenParen())
    case _   => None
  }
  
  override protected def parseWord(word: String): Token[ComplexFunction] = word.toLowerCase match {
    case "i"        => Constant(Complex.I)
    case "pi"|"π"   => Constant(Complex.Pi)
    case "tau"|"τ"  => Constant(Complex.Tau)
    case "e"        => Constant(Complex.E)
    case "im"       => IM
    case "re"       => RE
    case "arg"      => ARG
    case "abs"      => ABS
    case "sqrt"|"√" => SQRT
    case "log"|"ln" => LOG
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
    case _          => try {
      Constant(word.toDouble)
    } catch {
      case _: NumberFormatException => Variable(word)
    }
  }
}