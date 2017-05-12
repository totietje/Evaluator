package net.totietje.evaluator

case class EvaluationException(message: String = "Syntax error", cause: Throwable = null)
  extends Exception(message, cause)
