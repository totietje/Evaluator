package net.totietje.evaluator

/** This is thrown by an [[net.totietje.evaluator.Evaluator Evaluator]] when it is parsing an expression with
  * a syntax error.
  * @param message
  *                The detailed message
  * @param cause
  *              The cause
  */
case class EvaluationException(message: String = "Syntax error", cause: Throwable = null)
  extends Exception(message, cause)
