package net.totietje.complex

import net.totietje.complex.ComplexFunctionToken._
import net.totietje.evaluator.Token._
import net.totietje.evaluator.{AbstractEvaluator, AbstractTokenizer, Token, Tokenizer}

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
  *   - conj
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
  override protected def tokenizer: Tokenizer[ComplexFunction] = ComplexTokenizer
}