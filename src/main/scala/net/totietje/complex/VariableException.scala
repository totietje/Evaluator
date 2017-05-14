package net.totietje.complex

/** An exception thrown by a [[net.totietje.complex.ComplexFunction ComplexFunction]] on evaluation if
  * an undefined variable is encountered.
  * @param message
  *                The detailed message
  * @param cause
  *              The cause
  */
case class VariableException(message: String = null, cause: Throwable = null) extends Exception(msg, cause)