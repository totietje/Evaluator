package net.totietje.evaluator

/** An `Evaluator` is a string parser and evaluator.
  *
  * This class is flexible, allowing a user to define their own syntax. An example use might be to parse maths
  * expressions, such as `(1 + 2) ^ 3`.
  *
  * @see [[net.totietje.complex.ComplexEvaluator ComplexEvaluator]]
  * @tparam R
  *           The type that the string input should be evaluated to
  */
abstract class Evaluator[+R] {
  
  /** Evaluates a string.
    *
    * @throws net.totietje.evaluator.EvaluationException
    *                                                    If there is a syntax error in the expression
    * @param in
    *                   The input string to parse
    * @return
    *         The result of parsing the input
    */
  def evaluate(in: String): R
}