package net.totietje.evaluator

import collection.immutable.IndexedSeq

/** Represents one of the base parts of an expression parsed by an [[net.totietje.evaluator.Evaluator Evaluator]].
  * @tparam R
  *           The type that the evaluator should return after evaluating the tokens
  */
sealed trait Token[R]

/** Contains all the different types of tokens.
  */
object Token {
  private[evaluator] sealed trait Postfix[R] extends Token[R]
  
  private[evaluator] sealed trait Precedence[R] extends Token[R] {
    def precedence : Int
  }
  
  abstract class Operator[R](val precedence: Int, val associativity: Associativity)
      extends Postfix[R] with Precedence[R] {
    def apply(left: R, right: R) : R
  }
  
  abstract class Function[R](val args: Int) extends Precedence[R] with Postfix[R] {
    override def precedence: Int = Int.MaxValue
    
    def apply(args: IndexedSeq[R]): R
  }
  
  abstract class Value[R] extends Postfix[R] {
    def apply(): R
  }
  
  /** Either an open or close parenthesis.
    * @tparam R
    *           The type that the evaluator should return after evaluating the tokens
    */
  sealed abstract class Parenthesis[R] extends Token[R] with Precedence[R] {
    override def precedence: Int = Int.MinValue
  }
  
  case class ArgSeparator[R]() extends Token[R]
  case class OpenParen[R]() extends Parenthesis[R]
  case class CloseParen[R]() extends Parenthesis[R]
}

sealed trait Associativity

object Associativity {
  case object LEFT extends Associativity
  case object RIGHT extends Associativity
}