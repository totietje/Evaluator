package net.totietje.complex

case class VariableException(msg: String = null, cause: Throwable = null) extends Exception(msg, cause)