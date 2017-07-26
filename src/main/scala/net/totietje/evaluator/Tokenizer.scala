package net.totietje.evaluator

/** This tokenizes a string, breaking it down into its principle parts. This may be useful when parsing a string when
  * overriding the [[net.totietje.evaluator.Evaluator Evaluator]] trait, particularly its
  * [[net.totietje.evaluator.AbstractEvaluator AbstractEvaluator]].
  *
  * @tparam R
  *           The type of token that the string should be evaluated to
  */
trait Tokenizer[R] {
  
  /** Breaks down a string into a Seq of [[net.totietje.evaluator.Token Tokens]], which
    * represent the string's principle parts.
    * @throws net.totietje.evaluator.EvaluationException
    *                                                    If the input string contains an invalid token
    * @param in
    *           The string to convert
    * @return
    *         A `Seq[Token[R]]` representing the string's principle parts
    */
  def tokenize(in: String): Seq[Token[R]]
}
