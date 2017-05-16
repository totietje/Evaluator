package net.totietje.complex

import ComplexFunctionToken._
import net.totietje.evaluator.Token._
import net.totietje.evaluator.{AbstractEvaluator, Token}

/** An [[net.totietje.evaluator.Evaluator Evaluator]] which parses an expression as a
  * [[net.totietje.complex.ComplexFunction ComplexFunction]].
  *
  * This allows the basic operators `+`, `-`, `*`, `/` and `&#94;` and the use of brackets.
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
  
  override protected def parseAfterValueChar(char: Char): Option[Token[ComplexFunction]] = char match {
    case '+' => Some(Plus)
    case '-' => Some(Minus)
    case '*' => Some(Multiply)
    case '/' => Some(Divide)
    case '^' => Some(Power)
    case ')' => Some(CloseParen())
    case _   => None
  }
  
  override protected def parseOtherChar(op: Char): Option[Token[ComplexFunction]] = op match {
    case '+' => Some(UnaryPlus)
    case '-' => Some(UnaryMinus)
    case '(' => Some(OpenParen())
    case _   => None
  }
  
  override protected def parseWord(word: String): Token[ComplexFunction] = word.toLowerCase match {
    case "i"        => Constant(Complex.I)
    case "pi"|"π"   => Constant(Complex.Pi)
    case "tau"|"τ"  => Constant(Complex.Tau)
    case "e"        => Constant(Complex.E)
    case "im"       => Im
    case "re"       => Re
    case "arg"      => Arg
    case "abs"      => Abs
    case "sqrt"|"√" => Sqrt
    case "log"|"ln" => Log
    case "sin"      => Sin
    case "asin"     => Asin
    case "cos"      => Cos
    case "acos"     => Acos
    case "tan"      => Tan
    case "atan"     => Atan
    case "sinh"     => Sinh
    case "asinh"    => Asinh
    case "cosh"     => Cosh
    case "acosh"    => Acosh
    case "tanh"     => Tanh
    case "atanh"    => Atanh
    case _          => try {
      Constant(word.toDouble)
    } catch {
      case _: NumberFormatException => Variable(word)
    }
  }
}